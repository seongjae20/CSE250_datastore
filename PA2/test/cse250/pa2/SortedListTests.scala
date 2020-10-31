/**
 * cse250.pa2.SortedListTests.scala
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

import cse250.adaptors.LectureQueue
import org.scalatest.{BeforeAndAfter, FlatSpec}
import org.scalatest.Assertions._

class SortedListTests extends FlatSpec with BeforeAndAfter {
  behavior of "insert"
  it should "insert a solo element into list at index 0" in {
    val myList = new SortedList[Int]
    val valToInsert = 5
    myList.insert(valToInsert)
    assert(myList.length == 1)
    assert(myList(0) == valToInsert)
  }

  it should "insert multiple elements into list from 0 to 10" in {
    val myList = new SortedList[Int]
    var length = 0
    for(idx <- 0 to 100){
      myList.insert(idx)
      length += 1
      assert(myList.length == length)
      assert(myList(idx) == idx)
    }
  }

  it should "insert a solo element into list at middle without same elem" in {
    val myList = new SortedList[Int]
    val Input_1 = 1
    val Input_2 = 2
    val Input_3 = 3
    // List(1)
    myList.insert(Input_1)
    // List(1,3)
    myList.insert(Input_3)
    // List(1,2,3)
    myList.insert(Input_2)
    assert(myList.length == 3)
    assert(myList(0) == Input_1)
    assert(myList(1) == Input_2)
    assert(myList(2) == Input_3)
  }

  it should "insert element into list by non-decreasing" in {
    val myList = new SortedList[Int]
    // List(1)
    myList.insert(1)
    assert(myList.length == 1)
    assert(myList.apply(0) == 1)
    // List(0,1)
    myList.insert(0)
    assert(myList.length == 2)
    assert(myList.apply(0) == 0)
    assert(myList.apply(1) == 1)
  }

  it should "insert a solo element into list at middle with same elem" in {
    val myList = new SortedList[Int]
    val Input_1 = 1
    val Input_2 = 2
    val Input_3 = 3
    // List(1)
    myList.insert(Input_1)
    // List(1,3)
    myList.insert(Input_3)
    // List(1,2,3)
    myList.insert(Input_2)
    // List(1,2,2,3)
    myList.insert(Input_2)
    assert(myList.length == 4)
    assert(myList(0) == Input_1)
    assert(myList(1) == Input_2)
    assert(myList(2) == Input_2)
    assert(myList(3) == Input_3)
    // List(1,2,2,2,3)
    myList.insert(Input_2)
    assert(myList.length == 5)
    assert(myList(0) == Input_1)
    assert(myList(1) == Input_2)
    assert(myList(2) == Input_2)
    assert(myList(3) == Input_2)
    assert(myList(4) == Input_3)
    // List(1,2,2,2,3,3)
    myList.insert(Input_3)
    assert(myList.length == 6)
    assert(myList(0) == Input_1)
    assert(myList(1) == Input_2)
    assert(myList(2) == Input_2)
    assert(myList(3) == Input_2)
    assert(myList(4) == Input_3)
    assert(myList(5) == Input_3)
    // List(1,1,2,2,2,3,3)
    myList.insert(Input_1)
    assert(myList.length == 7)
    assert(myList(0) == Input_1)
    assert(myList(1) == Input_1)
    assert(myList(2) == Input_2)
    assert(myList(3) == Input_2)
    assert(myList(4) == Input_2)
    assert(myList(5) == Input_3)
    assert(myList(6) == Input_3)
  }

  behavior of "processBatch"
  it should "process two insertions" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("insert",0)
    myList.processBatch(jobQueue)
    // Should have inserted the values: 0,0.
    assert(myList.length == 2)
    assert(myList(0) == 0)
    assert(myList(1) == 0)
    // Should have removed both copies of 0.
    jobQueue.enqueue("remove",0)
    myList.processBatch(jobQueue)
    assert(myList.length == 0)
  }

  it should "process two insertions and then undo both" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("insert",0)
    myList.processBatch(jobQueue)
    // Should have inserted the values: 0,0.
    assert(myList.length == 2)
    assert(myList(0) == 0)
    assert(myList(1) == 0)
    // Should have removed both copies of 0.
    myList.undoLastModification()
    assert(myList.length == 0)
  }

  it should "insert 0 and remove 1 then undo" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("remove",1)
    myList.processBatch(jobQueue)
    // List(0)
    assert(myList.length == 1)
    assert(myList(0) == 0)
    myList.undoLastModification()
    assert(myList.length == 0)
  }

  it should "insert 0 and remove 0 then undo" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("remove",0)
    myList.processBatch(jobQueue)
    // List()
    assert(myList.length == 0)
  }

  it should "process one insertion and then undo two times" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    // first time
    myList.processBatch(jobQueue)
    // Should have inserted the values: 0,0.
    assert(myList.length == 1)
    assert(myList(0) == 0)
    // Should have removed both copies of 0.
    myList.undoLastModification()
    assert(myList.length == 0)
    // second time
    jobQueue.enqueue("insert",0)
    myList.processBatch(jobQueue)
    // Should have inserted the values: 0,0.
    assert(myList.length == 1)
    assert(myList(0) == 0)
    // Should have removed both copies of 0.
    myList.undoLastModification()
    assert(myList.length == 0)
  }

  it should "two inesert Queue and two remove Queue" in {
    val myList = new SortedList[Int]
    val jobQueue_1 = new LectureQueue[(String,Int)]
    val jobQueue_2 = new LectureQueue[(String,Int)]
    jobQueue_1.enqueue("insert",0)
    jobQueue_1.enqueue("insert",1)
    jobQueue_2.enqueue("remove",0)
    jobQueue_2.enqueue("remove",1)
    // insert Queue
    myList.processBatch(jobQueue_1)
    // should be List(0,1)
    assert(myList.length == 2)
    assert(myList(0) == 0)
    assert(myList(1) == 1)
    // remove Queue
    myList.processBatch(jobQueue_2)
    // should be List()
    assert(myList.length == 0)
  }

  it should "many insert and remove at once" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    // List(0,1,1,2)
    myList.insert(0)
    myList.insert(1)
    myList.insert(1)
    myList.insert(2)
    jobQueue.enqueue("remove",1)
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("remove",0)
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("insert",3)
    // List(0,1,1,2) > List(0,2) > List(0,1,2) > List(0,0,1,2) > List(1,2) > List(1,1,2) > List(1,1,2,3)
    myList.processBatch(jobQueue)
    assert(myList.length == 4)
    assert(myList(0) == 1)
    assert(myList(1) == 1)
    assert(myList(2) == 2)
    assert(myList(3) == 3)
  }

  it should "remove and insert Queue then undo" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("remove",0)
    jobQueue.enqueue("insert",0)
    // List(0)
    myList.insert(0)
    // remove 0 and then insert 0, so List(0)
    myList.processBatch(jobQueue)
    // undo insert(0)
    myList.undoLastModification()
    assert(myList.length == 0)
  }

  behavior of "apply"
  it should "retrieve one single element" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    assert(myList.apply(0) == 0)
  }

  it should "retrieve multiple elements at correct index" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(1)
    myList.insert(2)
    assert(myList.apply(0) == 0)
    assert(myList.apply(1) == 1)
    assert(myList.apply(2) == 2)
  }

  it should "retrieve elements after modification" in {
    val myList = new SortedList[Int]
    // List(0,1)
    myList.insert(0)
    myList.insert(1)
    assert(myList.apply(0) == 0)
    assert(myList.apply(1) == 1)
    // List(1)
    myList.remove(0)
    assert(myList.apply(0) == 1)
    // List (0,1)
    myList.insert(0)
    assert(myList.apply(0) == 0)
    assert(myList.apply(1) == 1)
  }

  it should "retreive elements after undo" in {
    val myList = new SortedList[Int]
    // List() > List(0) > List(0,1) > List(0,1,2) > List(1,2) > List(2)
    // List(2)
    myList.insert(0)
    myList.insert(1)
    myList.insert(2)
    myList.remove(0)
    myList.remove(1)
    assert(myList.length == 1)
    assert(myList.apply(0) == 2)
    // List (1,2)
    myList.undoLastModification()
    assert(myList.length == 2)
    assert(myList.apply(0) == 1)
    assert(myList.apply(1) == 2)
    // List (0,1,2)
    myList.undoLastModification()
    assert(myList.length == 3)
    assert(myList.apply(0) == 0)
    assert(myList.apply(1) == 1)
    assert(myList.apply(2) == 2)
    // List (0,1)
    myList.undoLastModification()
    assert(myList.length == 2)
    assert(myList.apply(0) == 0)
    assert(myList.apply(1) == 1)
    // List (0)
    myList.undoLastModification()
    assert(myList.length == 1)
    assert(myList.apply(0) == 0)
    // List()
    myList.undoLastModification()
    assert(myList.length == 0)
  }

  it should "thrown with wrong index" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(1)
    assertThrows[IllegalArgumentException](myList.apply(2) == 0)
    assertThrows[IllegalArgumentException](myList.apply(3) == 0)
  }

  behavior of "remove"
  it should "remove nothing" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(1)
    myList.remove(2)
    assert(myList.length == 2)
    assert(myList(0) == 0)
    assert(myList(1) == 1)
  }

  it should "remove one element" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(1)
    myList.remove(1)
    assert(myList.length == 1)
    assert(myList(0) == 0)
  }

  it should "remove multiple elements" in {
    val myList = new SortedList[Int]
    // List(0,1,1,1)
    myList.insert(0)
    myList.insert(1)
    myList.insert(1)
    myList.insert(1)
    // List(0,1,1,1) -> remove 1 -> List(0)
    myList.remove(1)
    assert(myList.length == 1)
    assert(myList(0) == 0)
  }

  behavior of "length"
  it should "return length with emptylist" in {
    val myList = new SortedList[Int]
    assert(myList.length == 0)
  }

  it should "return length with 1 element" in{
    val myList = new SortedList[Int]
    myList.insert(1)
    assert(myList.length == 1)
  }

  it should "return length after insert and remove" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.remove(0)
    assert(myList.length == 0)
  }

  it should "return length with multiple elements" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(1)
    myList.insert(2)
    assert(myList.length == 3)
  }

  it should "return length with many operations" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(1)
    myList.remove(0)
    myList.insert(2)
    myList.insert(0)
    assert(myList.length == 3)
  }

  behavior of "undoLastModification"
  it should "undo nothing" in {
    val myList = new SortedList[Int]
    assertThrows[IllegalArgumentException](myList.undoLastModification())
  }

  it should "undo insert" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(1)
    myList.undoLastModification()
    assert(myList.length == 1)
    assert(myList(0) == 0)
  }

  it should "undo remove" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(1)
    myList.remove(1)
    myList.undoLastModification()
    assert(myList.length == 2)
    assert(myList(0) == 0)
    assert(myList(1) == 1)
  }

  it should "undo Queue" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("insert",2)
    myList.insert(0)
    myList.processBatch(jobQueue)
    myList.undoLastModification()
    assert(myList.length == 1)
    assert(myList(0) == 0)
  }

  it should "undo multiple insertion" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("insert",2)
    myList.insert(0)
    myList.processBatch(jobQueue)
    myList.undoLastModification()
    assert(myList.length == 1)
    assert(myList(0) == 0)
  }

  it should "undo multiple times" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("insert",2)
    myList.insert(0)
    myList.processBatch(jobQueue)
    myList.insert(1)
    myList.insert(2)
    myList.remove(1)
    // List() > List(0) > List(0,1,2) > List(0,1,1,2) > List(0,1,1,2,2) > List(0,2,2)
    // undo remove 1
    myList.undoLastModification()
    assert(myList.length == 5)
    assert(myList(0) == 0)
    assert(myList(1) == 1)
    assert(myList(2) == 1)
    assert(myList(3) == 2)
    assert(myList(4) == 2)
    // undo insert 2
    myList.undoLastModification()
    assert(myList.length == 4)
    assert(myList(0) == 0)
    assert(myList(1) == 1)
    assert(myList(2) == 1)
    assert(myList(3) == 2)
    // undo insert 1
    myList.undoLastModification()
    assert(myList.length == 3)
    assert(myList(0) == 0)
    assert(myList(1) == 1)
    assert(myList(2) == 2)
    // undo Queue
    myList.undoLastModification()
    assert(myList.length == 1)
    assert(myList(0) == 0)
    // undo insert 0
    myList.undoLastModification()
    assert(myList.length == 0)
  }

  behavior of "iterator"
  it should "retrieve all elements" in {
    val myList = new SortedList[Int]
    val test_myList = new SortedList[Int]
    val _storage = new SortedList[Int]
    for(num <- 0 to 9){
      myList.insert(num)
      test_myList.insert(num)
    }
    val iterator = myList._storageList.iterator
    for(_ <- 0 until myList.length){
      assert(iterator.hasNext)
      val elem = iterator.next()
      assert(test_myList.contains(elem))
      if(_storage.length > 0) {
        assert(_storage(_storage.length - 1) <= elem)
      }
      _storage.insert(elem)
    }
    assert(!iterator.hasNext)
    val elem = iterator.next()
    assert(test_myList.contains(elem))
    assert(_storage(_storage.length - 1) <= elem)
    _storage.insert(elem)
  }
}
