package scala.swing.tree

import Tree.Path
import scala.collection.mutable.ListBuffer
import javax.swing.{tree => jst}
import javax.swing.event.TreeModelEvent
import javax.swing.event.TreeModelListener
import scala.collection.mutable.ArrayBuffer


object ExternalTreeModel {
  def empty[A]: ExternalTreeModel[A] = new ExternalTreeModel[A](Seq.empty, _ => Seq.empty)
  def apply[A](roots: A*)(children: A => Seq[A]): ExternalTreeModel[A] = 
      new ExternalTreeModel(roots, children)
}

/**
 * Represents tree data as a sequence of root nodes, and a function that can retrieve child nodes.  
 */
class ExternalTreeModel[A](rootItems: Seq[A], 
                   children: A => Seq[A]) extends TreeModel[A] {
  self =>
  
  import TreeModel._
  
  private var rootsVar = List(rootItems: _*)
  
  def roots: Seq[A] = rootsVar
  
  def getChildrenOf(parentPath: Path[A]): Seq[A] = if (parentPath.isEmpty) roots 
                                                   else children(parentPath.last)
  
  def filter(p: A => Boolean): ExternalTreeModel[A] = new ExternalTreeModel[A](roots filter p, a => children(a) filter p) 

  def toInternalModel: InternalTreeModel[A] = InternalTreeModel(roots: _*)(children)
  
  def isExternalModel = true
  
  def map[B](f: A=>B): InternalTreeModel[B] = toInternalModel map f
  
  def pathToTreePath(path: Tree.Path[A]): jst.TreePath = {
    val array = (hiddenRoot +: path).map(_.asInstanceOf[AnyRef]).toArray(ClassManifest.Object)
    new jst.TreePath(array)
  }
  
  def treePathToPath(tp: jst.TreePath): Tree.Path[A] = {
    if (tp == null) null 
    else tp.getPath.map(_.asInstanceOf[A]).tail.toIndexedSeq
  } 
  
  /** 
   * A function to update a value in the model, at a given path.  By default this will throw an exception; to 
   * make a TreeModel updatable, call makeUpdatable() to provide a new TreeModel with the specified update method.
   */
  protected[tree] val updateFunc: (Path[A], A) => A = {
    (_,_) => error("Update is not supported on this tree")
  }
  
  /** 
   * A function to insert a value in the model at a given path, returning whether the operation succeeded.  
   * By default this will throw an exception; to allow insertion on a TreeModel, 
   * call insertableWith() to provide a new TreeModel with the specified insert method.
   */
  protected[tree] val insertFunc: (Path[A], A, Int) => Boolean = {
    (_,_,_) => error("Insert is not supported on this tree")
  }

  /** 
   * A function to remove a item in the model at a given path, returning whether the operation succeeded.  
   * By default this will throw an exception; to allow removal from a TreeModel, 
   * call removableWith() to provide a new TreeModel with the specified remove method.
   */
  protected[tree] val removeFunc: Path[A] => Boolean = {
    _ => error("Removal is not supported on this tree")
  }
  
  /**
   * Returns a new VirtualTreeModel that knows how to modify the underlying representation, 
   * using the given function to replace one value with another.   
   * <p>
   * Calling update() on a model returned from makeUpdatable() will perform the update.
   */
  def makeUpdatableWith(effectfulUpdate: (Path[A], A) => A): ExternalTreeModel[A] = new ExternalTreeModel(roots, children) {
    override val updateFunc = effectfulUpdate
    override val insertFunc = self.insertFunc
    override val removeFunc = self.removeFunc
    this.peer copyListenersFrom self.peer
  }

  def makeInsertableWith(effectfulInsert: (Path[A], A, Int) => Boolean): ExternalTreeModel[A] = new ExternalTreeModel(roots, children) {
    override val updateFunc = self.updateFunc
    override val insertFunc = effectfulInsert
    override val removeFunc = self.removeFunc
    this.peer copyListenersFrom self.peer
  }
  
  def makeRemovableWith(effectfulRemove: Path[A] => Boolean): ExternalTreeModel[A] = new ExternalTreeModel(roots, children) {
    override val updateFunc = self.updateFunc
    override val insertFunc = self.insertFunc
    override val removeFunc = effectfulRemove
    this.peer copyListenersFrom self.peer
  }
  
  /**
   * Replaces one value with another, mutating the underlying structure.  
   * If a way to modify the external tree structure has not been provided with makeUpdatableWith(), then
   * an exception will be thrown.
   */
  def update(path: Path[A], newValue: A) {
    if (path.isEmpty) throw new IllegalArgumentException("Cannot update an empty path")
    
    val existing = path.last
    val result = updateFunc(path, newValue)

    val replacingWithDifferentReference = existing.isInstanceOf[AnyRef] && 
                                          (existing.asInstanceOf[AnyRef] ne result.asInstanceOf[AnyRef])
       
    
    // If the result is actually replacing the node with a different reference object, then 
    // fire "tree structure changed".
    if (replacingWithDifferentReference) {
      if (path.size == 1) {
        rootsVar = rootsVar.updated(roots indexOf newValue, newValue)
      }
      
      peer.fireTreeStructureChanged(pathToTreePath(path.init), result)
    }
    // If the result is a value type or is a modification of the same node reference, then
    // just fire "nodes changed".
    else {
      peer.fireNodesChanged(pathToTreePath(path.init), result)
    }
  }
  
  def insertUnder(parentPath: Path[A], newValue: A, index: Int): Boolean = {
    val succeeded = if (parentPath.nonEmpty) {
      insertFunc(parentPath, newValue, index)
    }
    else { 
      val (before, after) = rootsVar splitAt index
      rootsVar = before ::: newValue :: after
      true 
    }
                                                     
    if (succeeded) {
      val actualIndex = siblingsUnder(parentPath) indexOf newValue
      if (actualIndex == -1) return false
        
      peer.fireNodesInserted(pathToTreePath(parentPath), newValue, actualIndex)
    }
    succeeded
  }
  
  def remove(pathToRemove: Path[A]): Boolean = {
    if (pathToRemove.isEmpty) return false
    
    val parentPath = pathToRemove.init
    val index = siblingsUnder(parentPath) indexOf pathToRemove.last
    if (index == -1) return false
      
    val succeeded = if (pathToRemove.size == 1) {
      rootsVar = rootsVar.filterNot(pathToRemove.last ==)
      true
    }
    else {
      removeFunc(pathToRemove)
    }
    
    if (succeeded) {

      peer.fireNodesRemoved(pathToTreePath(parentPath), pathToRemove.last, index)
    }
    succeeded
  }

  
  class ExternalTreeModelPeer extends jst.TreeModel {
    private val treeModelListenerList = ListBuffer[TreeModelListener]()

    def getChildrenOf(parent: Any) = parent match {
      case `hiddenRoot` => roots
      case a: A => children(a)
    }
    
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
    
    
    private[tree] def copyListenersFrom(otherPeer: ExternalTreeModel[A]#ExternalTreeModelPeer) {
      otherPeer.treeModelListeners foreach addTreeModelListener
    }
    
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
    
    private def createEvent(parentPath: jst.TreePath, newValue: Any) = {
      val index = getChildrenOf(parentPath.getPath.last) indexOf newValue
      createEventWithIndex(parentPath, newValue, index)
    }
  
    private def createEventWithIndex(parentPath: jst.TreePath, newValue: Any, index: Int) = {
      new TreeModelEvent(this, parentPath, Array(index), Array(newValue.asInstanceOf[AnyRef]))
    }
    
    def fireTreeStructureChanged(parentPath: jst.TreePath, newValue: Any) {
      treeModelListenerList foreach { _ treeStructureChanged createEvent(parentPath, newValue) }
    }
    
    def fireNodesChanged(parentPath: jst.TreePath, newValue: Any) {
      treeModelListenerList foreach { _ treeNodesChanged createEvent(parentPath, newValue) }
    }
    
    def fireNodesInserted(parentPath: jst.TreePath, newValue: Any, index: Int) {
      def createEvent = createEventWithIndex(parentPath, newValue, index)
      treeModelListenerList foreach { _ treeNodesInserted createEvent }
    }
    
    def fireNodesRemoved(parentPath: jst.TreePath, removedValue: Any, index: Int) {
      def createEvent = createEventWithIndex(parentPath, removedValue, index)
      treeModelListenerList foreach { _ treeNodesRemoved createEvent }
    }
  }
  
  
  
  /**
   * Underlying tree model that exposes the tree structure to Java Swing.
   *
   * This implementation of javax.swing.tree.TreeModel takes advantage of its abstract nature, so that it respects 
   * the tree shape of the underlying structure provided by the user.
   */
  lazy val peer = new ExternalTreeModelPeer
   
}

