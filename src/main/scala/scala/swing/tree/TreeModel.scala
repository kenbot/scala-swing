package scala.swing
package tree

import javax.swing.event.{TreeModelListener, TreeModelEvent}
import javax.swing.{tree => jst}
import java.{util => ju}
import scala.reflect.ClassManifest
import scala.collection.mutable.ListBuffer
import Tree.Path
import scala.swing.event._

object TreeModel {
  
  /**
   * This value is the root node of every TreeModel's underlying javax.swing.tree.TreeModel.  As we wish to support multiple root 
   * nodes in a typesafe manner, we need to maintain a permanently hidden dummy root to hang the user's "root" nodes off.
   */
  private[tree] case object hiddenRoot
 
  
  def empty[A] = new VirtualTreeModel[A](Seq.empty, _ => Seq.empty) // Needs to be a method rather than a val, because A is invariant.
  def apply[A](roots: A*)(children: A => Seq[A]) = new VirtualTreeModel(roots, children)
}

import TreeModel._

trait TreeModel[A] {
  
  val roots: Seq[A]
  val peer: jst.TreeModel 
  def getChildrenOf(parent: A): Seq[A]
  def filter(p: A => Boolean): TreeModel[A]
  def map[B](f: A=>B): TreeModel[B]
  
  def foreach[U](f: A=>U): Unit = depthFirstIterator foreach f
  def virtual: Boolean
  def toPhysicalModel: PhysicalTreeModel[A]
  
  def pathToTreePath(path: Tree.Path[A]) = {
    val array = (hiddenRoot :: path).map(_.asInstanceOf[AnyRef]).toArray(ClassManifest.Object)
    new jst.TreePath(array)
  }
  
  /** 
   * A function to update a value in the model, at a given path.  By default this will throw an exception; to 
   * make a TreeModel updatable, call updatableWith() to provide a new TreeModel with the specified update method.
   */
  //@deprecated("to be removed")
  protected val updateFunc: (Path[A], A) => A = {
    (_,_) => error("Update is not supported on this tree")
  }
  
  //@deprecated("to be removed")
  def update(path: Path[A], newValue: A) {
  /*
    val existing = path.last
    val result = updateFunc(path, newValue)

    // If the result is actually replacing the node with a different reference object, then 
    // fire "tree structure changed".
    if (existing.isInstanceOf[AnyRef] && (existing.asInstanceOf[AnyRef] ne result.asInstanceOf[AnyRef])) {
      peer.fireTreeStructureChanged(pathToTreePath(path), result)
    }
    // If the result is a value type or is a modification of the same node reference, then
    // just fire "nodes changed".
    else {
      peer.fireNodesChanged(pathToTreePath(path), result)
    }*/
  }

  /**
   * Returns a new TreeModel that is updatable with the given function. The returned TreeModel will have 
   * the same roots and child function.
   */
  //@deprecated("to be removed")
  def updatableWith(updater: (Path[A], A) => A): TreeModel[A] 

 
  def treePathToPath(tp: jst.TreePath): Tree.Path[A] = {
    if (tp == null) null 
    else tp.getPath.map(_.asInstanceOf[A]).toList.tail
  }   

  /**
   * Iterates sequentially through each item in the tree, either in breadth-first or depth-first ordering, 
   * as decided by the abstract pushChildren() method.
   */
  private trait TreeIterator extends Iterator[A] {
    protected var openNodes: Iterator[A] = roots.iterator

    def pushChildren(item: A): Unit
    def hasNext = openNodes.nonEmpty
    def next() = if (openNodes.hasNext) {
      val item = openNodes.next
      pushChildren(item)
      item
    }
    else error("No more items")
  }
  
  def breadthFirstIterator: Iterator[A] = new TreeIterator {
    override def pushChildren(item: A) {openNodes ++= getChildrenOf(item).iterator}
  }
  
  def depthFirstIterator: Iterator[A] = new TreeIterator {
    override def pushChildren(item: A) {
      val open = openNodes
      openNodes = getChildrenOf(item).iterator ++ open // ++'s argument is by-name, and should not directly pass in a var
    }
  }
  
  def size = depthFirstIterator.size

  trait ModelPeer extends jst.TreeModel {
    private val treeModelListenerList = ListBuffer[TreeModelListener]()

    def getChildrenOf(parent: Any): Seq[A]
    
    def getChild(parent: Any, index: Int): AnyRef = {
      val ch = getChildrenOf(parent)
      if (index >= 0 && index < ch.size) 
        ch(index).asInstanceOf[AnyRef] 
      else 
        error("No child of \"" + parent + "\" found at index " + index)
    }
    def getChildCount(parent: Any): Int = getChildrenOf(parent).size
    def getIndexOfChild(parent: Any, child: Any): Int = getChildrenOf(parent) indexOf child
    def getRoot(): AnyRef = hiddenRoot
    def isLeaf(node: Any): Boolean = getChildrenOf(node).isEmpty
    
    
    def treeModelListeners: Seq[TreeModelListener] = treeModelListenerList
    
    def addTreeModelListener(tml: TreeModelListener) {
      treeModelListenerList += tml
    }
    
    def removeTreeModelListener(tml: TreeModelListener) {
      treeModelListenerList -= tml
    }
    
    def valueForPathChanged(path: jst.TreePath, newValue: Any) {
      update(treePathToPath(path), newValue.asInstanceOf[A])
    }
    
    private def createEvent(path: jst.TreePath, newValue: Any) = new TreeModelEvent(this, path, 
        Array(getChildrenOf(path.getPath.last) indexOf newValue), 
        Array(newValue.asInstanceOf[AnyRef]))
    
    def fireTreeStructureChanged(path: jst.TreePath, newValue: Any) {
      treeModelListenerList foreach (_.treeStructureChanged(createEvent(path, newValue)))
    }
    
    def fireNodesChanged(path: jst.TreePath, newValue: Any) {
      treeModelListenerList foreach (_.treeNodesChanged(createEvent(path, newValue)))
    }
    
    def fireNodesInserted(path: jst.TreePath, newValue: Any, index: Int) {
      treeModelListenerList foreach (_.treeNodesInserted(createEvent(path, newValue)))
    }
  }
}

class PhysicalTreeModel[A] protected(val roots: Seq[A], childFunc: A => Seq[A]) extends TreeModel[A] {
  self =>
  
  // TODO: Make a constructor that can just takes a jst.TreeModel, so that 
  // map() and filter() can directly use the current values stored in the 

  val peer: jst.TreeModel = new jst.DefaultTreeModel(createRootNode)
  
  def this(roots: Seq[A]) = this(roots, _ => Seq.empty)
  
  def map[B](f: A=>B): TreeModel[B] = new PhysicalTreeModel[B](roots map f, _ => Seq.empty) {
    override val peer = copyFromModel(self, f)
  }
  

  protected[tree] def copyFromModel[B](otherModel: TreeModel[B], f: B => A): jst.TreeModel = {
    def copyNode(b: B): jst.MutableTreeNode = {
      val a = f(b)
      
      new jst.DefaultMutableTreeNode(a) {
        otherModel.getChildrenOf(b) map copyNode foreach add
      }
    }
    
    new jst.DefaultTreeModel(new jst.DefaultMutableTreeNode(hiddenRoot) {
      otherModel.roots map copyNode foreach add
    })
  }

  def getChildrenOf(parent: A): Seq[A] = childFunc(parent)
  def filter(p: A => Boolean) = new PhysicalTreeModel[A](roots filter p, a => childFunc(a) filter p) 
  def updatableWith(updater: (Path[A], A) => A): TreeModel[A] = this
    
  def toPhysicalModel: PhysicalTreeModel[A] = this
  
  def virtual = false  
   

  
  private def createRootNode(): jst.MutableTreeNode = new jst.DefaultMutableTreeNode(hiddenRoot) {
    def createNode(a: A): jst.MutableTreeNode = new jst.DefaultMutableTreeNode(a) {
      childFunc(a) map createNode foreach add
    }
    roots map createNode foreach add
  }
}
  /*
  class LazyNode(parentNode: jst.MutableTreeNode, val typedValue: A) extends jst.DefaultMutableTreeNode(typedValue) {
    override def getChildAt(childIndex: Int): jst.TreeNode = {
      initChildren();
      super.getChildAt(childIndex)
    }
    
    override def getChildCount(): Int = {
      initChildren();
      super.getChildCount()
    }
    
    override def getParent(): jst.TreeNode = {
      initChildren();
      super.getParent();
    }
    
    override def getIndex(node: jst.TreeNode): Int
    override def getAllowsChildren(): Boolean
    override def isLeaf(): Boolean
    override def insert(child: jst.MutableTreeNode, index: Int): Unit
    override def remove(index: Int): Unit
    override def remove(node: jst.MutableTreeNode): Unit
    override def setUserObject(Object object)
    override def removeFromParent(): Unit
    override def setParent(newParent: jst.MutableTreeNode): Unit
  
  
  
    protected def initChildren(): Unit = childFunc(nodeValue) map createNode foreach add
    
    override def children(): ju.Enumeration[_] = {
      initChildren()
      super.children()
    }*/


/**
 * Represents tree data as a sequence of root nodes, and a function that can retrieve child nodes.  
 */
class VirtualTreeModel[A](val roots: Seq[A], 
                   children: A => Seq[A]) extends TreeModel[A] {
  self =>
  
  import TreeModel._
 
  def getChildrenOf(parent: A): Seq[A] = children(parent)
  
  def filter(p: A => Boolean): VirtualTreeModel[A] = new VirtualTreeModel[A](roots filter p, a => children(a) filter p) 

  def toPhysicalModel: PhysicalTreeModel[A] = new PhysicalTreeModel(roots, children)
  
  def virtual = true
  
  def map[B](f: A=>B): TreeModel[B] = toPhysicalModel map f
  
  
  /**
   * Returns a new TreeModel that is updatable with the given function. The returned TreeModel will have 
   * the same roots and child function.
   */
  def updatableWith(updater: (Path[A], A) => A): TreeModel[A] = new VirtualTreeModel(roots, children) {
    override val updateFunc = updater
    this.peer.treeModelListeners foreach self.peer.addTreeModelListener
  }

 
  /**
   * Underlying tree model that exposes the tree structure to Java Swing.
   *
   * This implementation of javax.swing.tree.TreeModel takes advantage of its abstract nature, so that it respects 
   * the tree shape of the underlying structure provided by the user.
   */
  lazy val peer = new ModelPeer {

    def getChildrenOf(parent: Any) = parent match {
      case `hiddenRoot` => roots
      case a: A => children(a)
    }
  }
   
}

