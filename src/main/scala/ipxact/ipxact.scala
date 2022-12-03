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

package ipxact

import scala.collection.AbstractMap
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.xml.Node

trait ScalaGenerator {
  def scalaInstance(config: Map[String, String] = Map())(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[(String, Seq[String])]
}

trait ScalaDefinition {
  def scalaDefinition(tabDepth: Int = 0, config: Map[String, String] = Map())(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[(String, Seq[String])]
}

trait Versioned {
  val identifier: VersionedIdentifier
}

case class VersionedIdentifier(vendor: String,
                               library: String,
                               name: String,
                               version: String)

object VersionedIdentifier {
  def parse(node: Node): VersionedIdentifier = {
    VersionedIdentifier(
      vendor = (node \ "vendor").text,
      library = (node \ "library").text,
      name = (node \ "name").text,
      version = (node \ "version").text
    )
  }
}

case class LibraryRefType(vendor: String,
                          library: String,
                          name: String,
                          version: String)

object LibraryRefType {
  def parse(node: Node): LibraryRefType = {
    val spiritUri = node.scope.getURI("spirit")
    LibraryRefType(
      vendor = node \@ s"{$spiritUri}vendor",
      library = node \@ s"{$spiritUri}library",
      name = node \@ s"{$spiritUri}name",
      version = node \@ s"{$spiritUri}version"
    )
  }

  implicit def toVersionedIdentifier(ref: LibraryRefType): VersionedIdentifier = VersionedIdentifier(vendor = ref.vendor, library = ref.library, name = ref.name, version = ref.version)
}

sealed trait Qualifier

object Address extends Qualifier

object Data extends Qualifier

object Clock extends Qualifier

object Reset extends Qualifier

sealed trait PortStyle

object Wire extends PortStyle

object Transactional extends PortStyle

sealed trait Presence

object Required extends Presence

object Illegal extends Presence

object Optional extends Presence

trait WirePortDirection

object In extends WirePortDirection

object Out extends WirePortDirection

object InOut extends WirePortDirection

case class AbstractionDefinitionWirePort(presence: Presence,
                                         width: Option[Int],
                                         direction: WirePortDirection)

object AbstractionDefinitionWirePort {
  def parse(node: Node): AbstractionDefinitionWirePort = {
    AbstractionDefinitionWirePort(
      presence = (node \ "presence").headOption.map(_.text.toLowerCase).orNull match {
        case "required" => `Required`
        case "illegal" => `Illegal`
        case "optional" => `Optional`
        case _ => `Optional`
      },
      width = (node \ "width").headOption.map(_.text.toInt),
      direction = (node \ "direction").headOption.map(_.text.toLowerCase).orNull match {
        case "in" => `In`
        case "out" => `Out`
        case "inout" => `InOut`
        case _ => `Out`
      }
    )
  }
}

case class AbstractionDefinitionPort(logicalName: String,
                                     displayName: Option[String],
                                     description: Option[String],
                                     qualifier: Option[Qualifier],
                                     portStyle: PortStyle,
                                     onMaster: Option[AbstractionDefinitionWirePort],
                                     onSlave: Option[AbstractionDefinitionWirePort],
                                     defaultValue: Option[BigInt]) {
  def asMaster(): Option[(WirePortDirection, Int)] = {
    val dir = onMaster.map(_.direction).orElse(onSlave.map(_.direction.flip()))
    val width = onMaster.flatMap(_.width).orElse(onSlave.flatMap(_.width))
    if (dir.isDefined && width.isDefined)
      Some((dir.get, width.get))
    else
      None
  }
}

object AbstractionDefinitionPort {
  def parse(node: Node): AbstractionDefinitionPort = {
    AbstractionDefinitionPort(
      logicalName = (node \ "logicalName").head.text,
      displayName = (node \ "displayName").map(_.text).headOption,
      description = (node \ "description").map(_.text).headOption,
      qualifier = (node \ "qualifier").map(_.text.toLowerCase match {
        case "isAddress" => `Address`
        case "isData" => `Data`
        case "isClock" => `Clock`
        case "isReset" => `Reset`
        case _ => null
      }).headOption,
      portStyle = node.child.flatMap(_.label match {
        case "wire" => Some(`Wire`)
        case "transactional" => Some(`Transactional`)
        case _ => None
      }).headOption.get,
      onMaster = (node \ "onMaster").map(n => AbstractionDefinitionWirePort.parse(n)).headOption,
      onSlave = (node \ "onSlave").map(n => AbstractionDefinitionWirePort.parse(n)).headOption,
      defaultValue = (node \ "defaultValue").map(n => BigInt(n.text)).headOption
    )
  }
}

trait AbstractionDefinitionI extends Versioned with ScalaGenerator with ScalaDefinition

case class AbstractionDefinition(identifier: VersionedIdentifier,
                                 busType: LibraryRefType,
                                 extendsRefs: Seq[LibraryRefType],
                                 ports: Seq[AbstractionDefinitionPort]) extends AbstractionDefinitionI {
  override def scalaInstance(config: Map[String, String] = Map())(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[(String, Seq[String])] = {
    Some(s"${busType.name}()", Seq.empty)
  }

  override def scalaDefinition(tabDepth: Int, config: Map[String, String])(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[(String, Seq[String])] = {
    val fullyDefinedPorts = ports.filter(_.asMaster().isDefined).toList
    val valDefs: Seq[String] = fullyDefinedPorts.map(p => s"val ${p.logicalName} = Bits(${p.asMaster().get._2} bit)")
    val valDirs: Seq[String] = fullyDefinedPorts.map(p => s"${p.asMaster().get._1.toString}(${p.logicalName})")
    val tabStr = " " * tabDepth
    val scalaDef = s"""
       |case class ${busType.name}() extends Bundle with IMasterSlave {
       |${valDefs.map(s => s"${tabStr}${s}").mkString("\n")}
       |
       |${tabStr}override def asMaster(): Unit = {
       |${valDirs.map(s => s"${tabStr}${tabStr}${s}").mkString("\n")}
       |${tabStr}}
       |}
       |""".stripMargin
    Some(scalaDef, Seq())
  }
}

object AbstractionDefinition {
  def parse(node: Node): AbstractionDefinition = {
    AbstractionDefinition(
      identifier = VersionedIdentifier.parse(node),
      busType = (node \ "busType").map(n => LibraryRefType.parse(n)).head,
      extendsRefs = (node \ "extends").map(n => LibraryRefType.parse(n)),
      ports = (node \ "ports" \ "port").map(n => AbstractionDefinitionPort.parse(n))
    )
  }
}

case class BusDefinition(identifier: VersionedIdentifier,
                         directionConnection: Boolean,
                         addressable: Boolean,
                         extendsRefs: Seq[LibraryRefType] = Seq.empty,
                         maxMasters: Int = -1,
                         maxSlaves: Int = -1) extends Versioned

object BusDefinition {
  def parse(node: Node): BusDefinition = {
    BusDefinition(
      identifier = VersionedIdentifier.parse(node),
      directionConnection = (node \ "directConnection").text.toBoolean,
      addressable = (node \ "isAddressable").text.toBoolean,
      extendsRefs = (node \ "extends").map(n => LibraryRefType.parse(n)),
      maxMasters = (node \ "maxMasters").map(_.text.toInt).headOption.getOrElse(-1),
      maxSlaves = (node \ "maxMasters").map(_.text.toInt).headOption.getOrElse(-1)
    )
  }
}

case class NameGroup(name: String,
                     displayName: Option[String],
                     description: Option[String])

object NameGroup {
  def parse(node: Node): NameGroup = {
    NameGroup(
      name = (node \ "name").text,
      displayName = (node \ "displayName").map(_.text).headOption,
      description = (node \ "description").map(_.text).headOption
    )
  }
}

//case class AddressSpaceRef()

trait InterfaceMode
case class Master() extends InterfaceMode
case class Slave() extends InterfaceMode
case class System() extends InterfaceMode
case class MirroredSlave() extends InterfaceMode
case class MirroredMaster() extends InterfaceMode
case class MirroredSystem() extends InterfaceMode
case class Monitor() extends InterfaceMode

object InterfaceMode {
  def parse(node: Node): InterfaceMode = {
    node.child.map { n =>
      n.label.toLowerCase match {
        case "master" => Master()
        case "slave" => Slave()
        case "system" => System()
        case "mirroredmaster" => MirroredMaster()
        case "mirroredslave" => MirroredSlave()
        case "mirroredsystem" => MirroredSystem()
        case "monitor" => Monitor()
        case _ => null
      }
    }.find(_ != null).orNull
  }
}

case class Vector()

object Vector {
  def parse(node: Node): Vector = {
    Vector()
  }
}

case class PortMap(logicalName: String,
                   logicalRange: Option[Vector],
                   physicalName: String,
                   physicalRange: Option[Vector])

object PortMap {
  def parse(node: Node): PortMap = {
    PortMap(
      logicalName = (node \ "logicalPort" \ "name").text,
      logicalRange = (node \ "logicalPort" \ "vector").map(n => Vector.parse(n)).headOption,
      physicalName = (node \ "physicalPort" \ "name").text,
      physicalRange = (node \ "physicalPort" \ "vector").map(n => Vector.parse(n)).headOption
    )
  }
}

case class Parameter()

object Parameter {
  def parse(node: Node): Parameter = {
    Parameter()
  }
}

case class VendorExtension()

object VendorExtension {
  def parse(node: Node): VendorExtension = {
    VendorExtension()
  }
}

case class BusInterface(nameGroup: NameGroup,
                        busType: LibraryRefType,
                        abstractionType: Option[LibraryRefType],
                        interfaceMode: InterfaceMode,
                        connectionRequired: Boolean = false,
                        portMaps: Seq[PortMap],
                        parameters: Seq[Parameter],
                        vendorExtensions: Seq[VendorExtension]) extends ScalaGenerator {
  override def scalaInstance(config: Map[String, String] = Map())(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[(String, Seq[String])] = {
    val busAbsDef = definitions(abstractionType.get).asInstanceOf[AbstractionDefinitionI]
    interfaceMode match {
      case _: Master => busAbsDef.scalaInstance(config).map {case (inst, libs) => (s"master(${inst})", libs)}
      case _: Slave => busAbsDef.scalaInstance(config).map {case (inst, libs) => (s"slave(${inst})", libs)}
      case _: MirroredMaster => busAbsDef.scalaInstance(config).map {case (inst, libs) => (s"slave(${inst})", libs)}
      case _: MirroredSlave => busAbsDef.scalaInstance(config).map {case (inst, libs) => (s"master(${inst})", libs)}
      case _: Monitor => busAbsDef.scalaInstance(config).map {case (inst, libs) => (s"in(${inst})", libs)}
      case _ => None
    }
  }

  def scalaLibraries(config: Map[String, String] = Map())(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Seq[String] = Seq.empty
}

object BusInterface {
  def parse(node: Node): BusInterface = {
    BusInterface(
      nameGroup = NameGroup.parse(node),
      busType = (node \ "busType").map(n => LibraryRefType.parse(n)).head,
      abstractionType = (node \ "abstractionType").map(n => LibraryRefType.parse(n)).headOption,
      interfaceMode = InterfaceMode.parse(node),
      connectionRequired = (node \ "connectionRequired").map(_.text.toBoolean).headOption.getOrElse(false),
      portMaps = (node \ "portMaps" \ "portMap").map(n => PortMap.parse(n)),
      parameters = (node \ "parameters" \ "parameter").map(n => Parameter.parse(n)),
      vendorExtensions = Seq.empty
    )
  }
}

case class Channel()
case class RemapState()
case class AddressSpace()
case class MemoryMap()
case class Model()
case class ComponentGenerator()
case class Choice()
case class FileSet()

case class Component(identifier: VersionedIdentifier,
                     busInterfaces: Seq[BusInterface],
                     channels: Seq[Channel],
                     remapStates: Seq[RemapState],
                     addressSpaces: Seq[AddressSpace],
                     memoryMaps: Seq[MemoryMap],
                     model: Option[Model],
                     componentGenerators: Seq[ComponentGenerator],
                     choices: Option[Choice],
                     fileSets: Seq[FileSet],
                     description: Option[String],
                     parameters: Seq[Parameter],
                     vendorExtensions: Seq[VendorExtension]) extends Versioned

object Component {
  def parse(node: Node) = {
    Component(
      identifier = VersionedIdentifier.parse(node),
      busInterfaces = (node \ "busInterfaces" \ "busInterface").map(n => BusInterface.parse(n)),
      channels = Seq.empty,
      remapStates = Seq.empty,
      addressSpaces = Seq.empty,
      memoryMaps = Seq.empty,
      model = None,
      componentGenerators = Seq.empty,
      choices = None,
      fileSets = Seq.empty,
      description = None,
      parameters = Seq.empty,
      vendorExtensions = Seq.empty
    )
  }
}

case class ConfigurableElementValue(referenceId: String,
                                    value: String)

object ConfigurableElementValue {
  def parse(node: Node) = {
    val spiritUri = node.scope.getURI("spirit")
    ConfigurableElementValue(
      referenceId = (node \@ s"{$spiritUri}referenceId"),
      value = node.text
    )
  }

  implicit def toMap(e: ConfigurableElementValue): (String, String) = (e.referenceId, e.value)
}

case class ComponentInstance(instanceName: String,
                             componentRef: LibraryRefType,
                             configurableElementValues: Seq[ConfigurableElementValue]) extends ScalaDefinition {
  override def scalaDefinition(tabDepth: Int, funcConfig: Map[String, String] = Map())(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[(String, Seq[String])] = {
    val config = configurableElementValues.map { e => e.referenceId -> e.value }.toMap ++ funcConfig
    val component = definitions.get(componentRef).map(_.asInstanceOf[Component]).orNull
    val tabStr = " " * tabDepth
    val ports: Seq[(String, Seq[String])] = component.busInterfaces.flatMap { bus =>
      val busConfig = config.filter { case (k, _) => k.toLowerCase.startsWith(s"BUSIFPARAM_VALUE.${bus.nameGroup.name}".toLowerCase) }
        .map { case (k, v) => k.split('.').drop(2).mkString(".") -> v }
      bus.scalaInstance(busConfig).map { case (inst, libs) =>
        (s"${tabStr*2}val ${bus.nameGroup.name} = ${inst}", libs)
      }
    }
    Some(((Seq(s"case class ${instanceName}() extends BlackBox {",
      s"${tabStr}val io = new Bundle() {") ++ ports.map(_._1) ++ Seq(s"${tabStr}}", "}")).mkString("\n"), ports.flatMap(_._2)))
  }
}

object ComponentInstance {
  def parse(node: Node) = {
    ComponentInstance(
      instanceName = (node \ "instanceName").text,
      componentRef = (node \ "componentRef").map(n => LibraryRefType.parse(n)).head,
      configurableElementValues = (node \ "configurableElementValues" \ "configurableElementValue").map(n => ConfigurableElementValue.parse(n))
    )
  }

  implicit class ComponentInstanceRich(component: ComponentInstance) {
    val COMMON_IMPORTS: Seq[String] = Seq(
      "import spinal.core._",
      "import spinal.lib._"
    )
    implicit def makeFile(definitions: AbstractMap[VersionedIdentifier, Any], tabDepth: Int = 2): Option[String] = {
      component.scalaDefinition(tabDepth = tabDepth)(definitions).map { case (inst, libs) =>
        val imports = libs.toSet[String].map(lib => s"import ${lib}")
        (COMMON_IMPORTS ++ Seq("") ++ imports ++ Seq("", inst)).mkString("\n")
      }
    }
  }
}

case class Design(identifier: VersionedIdentifier,
                  componentInstances: Seq[ComponentInstance])

object Design {
  def parse(node: Node) = {
    Design(
      identifier = VersionedIdentifier.parse(node),
      componentInstances = (node \ "componentInstances" \ "componentInstance").map(n => ComponentInstance.parse(n))
    )
  }
}