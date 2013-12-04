package org.ucombinator.dalvik.android

import java.util.zip.ZipFile
import java.io.{File,FileOutputStream}
import org.ucombinator.dalvik.AST.DexReader
import org.ucombinator.dalvik.AST.ClassDef

/** The ApkReader is mostly a wrapper class for reading the contained dex file
  * within the APK file.  Eventually the idea is to have the APK file also read
  * in things like the manifest (to figure out permissions), the resource files
  * (to make them available during abstract interpretation), etc.
  */
class ApkReader(f:File) {
  def this(fn:String) = this(new File(fn))

  def readFile():Array[ClassDef] = {
    val zf = new ZipFile(f)
    val dexEntry = zf.getEntry("classes.dex")
    val dexTmpFile = File.createTempFile("tmp", ".dex")
    val dexIs = zf.getInputStream(dexEntry)
    val dexOs = new FileOutputStream(dexTmpFile)
    val buffer = new Array[Byte](4096)
    var bytesRead = 0

    do {
      bytesRead = dexIs.read(buffer)
      if (bytesRead > 0) dexOs.write(buffer, 0, bytesRead)
    } while(bytesRead > 0)

    dexIs.close
    dexOs.close

    new DexReader(dexTmpFile).readFile
  }
}
