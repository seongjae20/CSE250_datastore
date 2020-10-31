/**
 * cse250.pa2.SortedList.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: seongjae
 * Person#: 50255595
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa2

import cse250.list.{ImmutableLinkedList,EmptyList,ListNode}
import cse250.adaptors.{LectureQueue,LectureStack}
import cse250.objects.TaxParcel


class SortedList[A] (implicit _comp: Ordering[A]) extends collection.Seq[A] {
  // Updates the toString to mention our class name instead of Seq.
  override protected[this] def className = "SortedList"

  // Use _storageList to maintain the sorted list.
  var _storageList: cse250.list.ImmutableLinkedList[A] = cse250.list.EmptyList
  // ---------- MAKE CHANGES BELOW ----------
  // You may add member variables as you wish.
  var undo_stack: LectureStack[ImmutableLinkedList[A]] = new LectureStack[ImmutableLinkedList[A]]
  var undo_length: LectureStack[Int] = new LectureStack[Int]
  var undo_batch: LectureStack[LectureQueue[A]] = new LectureStack[LectureQueue[A]]
  var _listlength = 0

  /** Gets element at position idx within the list. */
  override def apply(idx: Int): A = {
    require(idx >= 0 && length > idx)
    _storageList.apply(idx)
  }

  /** Gets the number of elements stored within the list. */
  override def length: Int = {
    _listlength
  }

  /** Returns an Iterator that can be used only once. */
  override def iterator: Iterator[A] = {
    val iter = _storageList.iterator
    var current_idx = 0

    def hasNext: Boolean = {
      current_idx < length
    }

    def next(): A = {
      val current_elem = _storageList(current_idx)
      current_idx += 1
      current_elem
    }
    iter
  }

  /**
   * Inserts one copy of elem into the list in non-decreasing order.
   * @param elem element to be inserted.
   */
  def insert(elem: A): Unit = {
    val comp_len = length
    // empty
    if(_storageList == EmptyList){
      undo_stack.push(_storageList)
      undo_length.push(_listlength)
      _storageList = _storageList.inserted(0, elem)
      _listlength += 1
    }
    // Not empty
    else {
      if(_comp.gteq(elem, apply(length - 1))){
        undo_stack.push(_storageList)
        undo_length.push(_listlength)
        _storageList = _storageList.inserted(length, elem)
        _listlength += 1
      }
      for (idx <- 0 to length) {
        if(comp_len == length) {
          // middle
          if (_comp.lteq(elem, apply(idx))) {
            undo_stack.push(_storageList)
            undo_length.push(_listlength)
            _storageList = _storageList.inserted(idx, elem)
            _listlength += 1
          }
        }
      }
    }
  }

  /**
   * Removes all copies of elem from the list.
   * @param elem element to be removed.
   * @return the number of copies removed.
   */
  def remove(elem: A): Int = {
    val copied_list = _storageList
    val copied_len = _listlength
    var ret_num = 0
    if(_storageList.contains(elem)) {
      for (idx <- length - 1 to 0 by -1) {
        if (apply(idx) == elem) {
          ret_num += 1
          _storageList = _storageList.removed(idx)
        }
      }
    }
    _listlength -= ret_num
    if(copied_list != _storageList){
      undo_stack.push(copied_list)
      undo_length.push(copied_len)
    }
    ret_num
  }

  /** Takes in a queue of valid operations to perform. Each pair has the form:
   *      (OP,elem)
   *  where:
   *      OP will be the string "insert" or "remove"
   *      elem will be a value of type A to use as the argument to OP. */
  def processBatch(operations: cse250.types.mutable.QueueADT[(String,A)]): Unit = {
    val copied_list = _storageList
    val copied_len = _listlength
    while(!operations.isEmpty){
      if(operations.front._1 == "insert"){
        insert(operations.front._2)
        // pop last undo stack created by insert
        undo_stack.pop
        undo_length.pop
      }
      else if(operations.front._1 == "remove"){
        if(_storageList.contains(operations.front._2)) {
          remove(operations.front._2)
          // pop last undo stack created by remove
          undo_stack.pop
          undo_length.pop
        }
      }
      operations.dequeue
    }
    // push undo stack created by batch
    if(copied_list != _storageList){
      undo_length.push(copied_len)
      undo_stack.push(copied_list)
    }
  }

  /** Undo the last modification, if any change has been made.
   *  If no change to undo exists, raise an IllegalArgumentException.
   */
  def undoLastModification(): Unit = {
    require(!undo_stack.isEmpty && !undo_length.isEmpty)
    _storageList = undo_stack.top
    _listlength = undo_length.top
    undo_stack.pop
    undo_length.pop
  }
}

object NewIntOrdering extends Ordering[Int] {
  def compare(x: Int, y: Int): Int = {
    // both even
    if(x%2 == 0 && y%2 == 0) {
      if (x > y) {
        -1
      }
      else if (x == y) {
        0
      }
      else{
        1
      }
    }
    // both odd
    else if(x%2 == 1 && y%2 == 1) {
      if (x > y) {
        -1
      }
      else if (x == y) {
        0
      }
      else{
        1
      }
    }
    // x is even and y is odd
    else if(x%2 == 0 && y%2 == 1) {
      -1
    }
    // x is odd and y is even
    else{
      1
    }
  }
}

object TaxParcelStreetGroupingOrdering extends Ordering[TaxParcel] {
  def compare(x: TaxParcel, y: TaxParcel): Int = {
    if(x.parcelInfo("STREET") < y.parcelInfo("STREET")){
      -1
    }
    else if(x.parcelInfo("STREET") == y.parcelInfo("STREET")){
      0
    }
    else{
      1
    }
  }
}