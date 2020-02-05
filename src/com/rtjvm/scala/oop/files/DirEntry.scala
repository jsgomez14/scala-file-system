package com.rtjvm.scala.oop.files

abstract class DirEntry(val parentPath: String, val name: String) {
  def path: String = {
    val separatorIfNecessary =
      if(parentPath.equals(Directory.ROOT_PATH)) ""
      else Directory.SEPARATOR
    parentPath + separatorIfNecessary + name
  }

  def asDirectory: Directory

  def asFile: File

  def isDirectory: Boolean

  def isFile: Boolean

  def getType: String

}
