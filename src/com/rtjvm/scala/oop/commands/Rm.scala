package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.Directory
import com.rtjvm.scala.oop.filesystem.State

class Rm(name: String) extends Command {

  override def apply(state: State): State = {
    // 1. get working dir
    val wd = state.wd

    // 2. get abs path
    val absolutePath =
      if(name.startsWith(Directory.SEPARATOR)) name
      else if(wd.isRoot) wd.path + name
      else wd.path + Directory.SEPARATOR + name
    // 3. do checks
    if(absolutePath.equals(Directory.ROOT_PATH))
      state.setMessage("Nuclear war not supported yet!")
    else
      doRm(state, absolutePath)

  }

  def doRm(state: State, path: String): State = {

    //TODO implement find descendent in directory
    def rmHelper(currDir: Directory, path: List[String]): Directory = {
      if(path.isEmpty) currDir
      else if(path.tail.isEmpty) currDir.removeEntry(path.head)
      else {
        val nextDir = currDir.findEntry(path.head)
        if(!nextDir.isDirectory) currDir
        else {
          val newNextDir = rmHelper(nextDir.asDirectory, path.tail)
          if(newNextDir == nextDir) currDir
          else currDir.replaceEntry(path.head, newNextDir)
        }
      }
    }
    // 4. find entry to remove
    // 5. update structure like we do for mkdir
    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot: Directory = rmHelper(state.root, tokens)

    if(newRoot == state.root)
      state.setMessage(path+": no such file or directory")
    else
      State(newRoot, newRoot.findDescendant(state.wd.path.substring(1)))
  }

}
