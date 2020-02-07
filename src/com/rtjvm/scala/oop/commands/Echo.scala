package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{Directory, File}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Echo(args: Array[String]) extends Command {
  val APPEND = ">>"
  val OVERRIDE = ">"
  override def apply(state: State): State = {
    if (args.length == 1 ) state.setMessage(args(0))
    else if (args.length == 1) state.setMessage(args(0))
    else {
      val operator = args(args.length - 2)
      val filename = args(args.length - 1)
      val contents = createContent(args, args.length - 2)
      if (operator.equals(APPEND))
        doEcho(state, contents, filename, append = true)
      else if(operator.equals(OVERRIDE))
        doEcho(state, contents, filename, append = false)
      else
        state.setMessage(createContent(args, args.length))

    }
  }

  def getRootAfterEcho(currDir: Directory, path: List[String], contents: String, append: Boolean): Directory = {
    if(path.isEmpty) currDir
    else if (path.tail.isEmpty){
      val dirEntry = currDir.findEntry(path.head)
      if(dirEntry == null) currDir.addEntry(new File(currDir.path, path.head, contents))
      else if(dirEntry.isDirectory) currDir
      else
        if(append) currDir.replaceEntry(path.head, dirEntry.asFile.appendContents(contents))
        else currDir.replaceEntry(path.head, dirEntry.asFile.setContents(contents))
    } else {
      val nextDirectory = currDir.findEntry(path.head).asDirectory
      val newNextDirectory  = getRootAfterEcho(nextDirectory, path.tail, contents, append)

      if(newNextDirectory == nextDirectory) currDir
      else currDir.replaceEntry(path.head, newNextDirectory)
    }
  }

  def doEcho(state: State, contents: String, filename: String, append: Boolean): State = {
    if(filename.contains(Directory.SEPARATOR))
      state.setMessage("Echo: filename must not contains separators.")
    else{
      val newRoot: Directory = getRootAfterEcho(state.root, state.wd.getAllFoldersInPath :+ filename, contents, append)
      if(newRoot == state.root)
        state.setMessage(filename+ ": no such file")
      else
        State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
    }
  }

  //TOP-INDEX non inclusive
  def createContent(args: Array[String], topIndex: Int): String = {
    @tailrec
    def createContentHelper(currentIndex: Int, accumulator: String): String = {
      if(currentIndex >= topIndex) accumulator
      else createContentHelper(currentIndex +1, accumulator +" "+args(currentIndex))
    }
    createContentHelper(0, "")
  }
}
