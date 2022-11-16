/*
 *    Copyright(C) 2022 Aidan Klein
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program. If not, see
 *    <http://www.gnu.org/licenses/>.
 */

import scala.collection.mutable
import scala.reflect.io.Path._
import scala.reflect.io._
import scala.xml._

object IPReaderTest {

  def parse(elem: Elem): Option[Any] = elem.label match {
    case "abstractionDefinition" => Some(ipxact.AbstractionDefinition.parse(elem))
    case "busDefinition" => Some(ipxact.BusDefinition.parse(elem))
    case "component" => Some(ipxact.Component.parse(elem))
    case "design" => Some(ipxact.Design.parse(elem))
    case "parameterAbstractionDefinition" => Some(ipxact.xilinx.ParameterAbstractionDefinition.parse(elem))
    case _ => None
  }

  def parseFile(f: File): Option[Any] = {
    try {
      val root = XML.loadFile(f.toAbsolute.path)
      val xilUri = root.scope.getURI("xilinx")
      val spiritUrl = root.scope.getURI("spirit")
      if (xilUri != null || spiritUrl != null) {
        return parse(root)
      }
    } catch {
      case _: SAXParseException => println("Failed to parse XML")
    }
    None
  }

  def main(args: Array[String]): Unit = {
    val xilinxInstallDir = sys.env("XILINX_INSTALL_DIR").toDirectory
    val xilinxIpDir = (xilinxInstallDir / "data" / "ip").toDirectory
    val ipFilesIter = xilinxIpDir.deepFiles.filter(_.hasExtension("xml"))
    val xilinxRsbDir = (xilinxInstallDir / "data" / "rsb" / "busdef").toDirectory
    val rsbFilesIter = xilinxRsbDir.deepFiles.filter(_.hasExtension("xml"))
    val allFilesIter = ipFilesIter ++ rsbFilesIter

    val t0 = System.nanoTime
    val parsed: Iterator[Any] = allFilesIter.flatMap(parseFile)

    var definitions = mutable.HashMap.empty[ipxact.VersionedIdentifier, Any]

    definitions ++= parsed.filter(_.isInstanceOf[ipxact.Versioned]).map(d => d.asInstanceOf[ipxact.Versioned].identifier -> d)
    val t1 = System.nanoTime

    val numBuses = definitions.count { case (k, v) => v.isInstanceOf[ipxact.BusDefinition] }
    println(s"Loaded ${numBuses} bus definitions")
    val numAbstractions = definitions.count { case (k, v) => v.isInstanceOf[ipxact.AbstractionDefinition] }
    println(s"Loaded ${numAbstractions} abstractions")
    val numComponents = definitions.count { case (k, v) => v.isInstanceOf[ipxact.Component] }
    println(s"Loaded ${numComponents} components")
    val numParamAbsDef = definitions.count { case (k, v) => v.isInstanceOf[ipxact.xilinx.ParameterAbstractionDefinition] }
    println(s"Loaded ${numParamAbsDef} parameter abstractions")
    println(s"Time: ${(t1-t0)/1e9} s")
    println()

    definitions = definitions.map { case (k, v) => k match {
        case ipxact.VersionedIdentifier("xilinx.com", "interface", "aximm_rtl", _) => k -> ipxact.xilinx.NativeAxiMMAbstraction()
        case ipxact.VersionedIdentifier("xilinx.com", "signal", "clock_rtl", _) =>  k -> ipxact.xilinx.GenericVectorAbstraction()
        case ipxact.VersionedIdentifier("xilinx.com", "signal", "reset_rtl", _) =>  k -> ipxact.xilinx.GenericVectorAbstraction()
        case ipxact.VersionedIdentifier("xilinx.com", "signal", "interrupt_rtl", _) =>  k -> ipxact.xilinx.GenericVectorAbstraction()
        case ipxact.VersionedIdentifier("xilinx.com", "interface", "axis_rtl", _) => k -> ipxact.xilinx.NativeAxisAbstraction()
        case _ => k -> v
      }
    }

    val filePath = "examples/xilinx/zynq_ultra_ps_e_0_0.xci".toFile
    val design = parseFile(filePath).flatMap {
      case design: ipxact.Design => Some(design)
      case _ => None
    }.head

    for(inst <- design.componentInstances.head.makeFile(definitions)) {
      println(inst)
      val outFile = "build/xilinx/zynq_ultra_ps_e_0_0.scala".toFile
      outFile.parent.createDirectory()
      val writer = outFile.bufferedWriter()
      writer.write(inst)
      writer.close()
    }
  }
}
