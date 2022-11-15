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

package ipxact.xilinx

import scala.xml.Node
import scala.collection.AbstractMap
import ipxact._

case class ParameterAbstractionDefinition(identifier: VersionedIdentifier,
                                          parameters: Seq[ParameterAbstraction]) extends Versioned

object ParameterAbstractionDefinition {
  def parse(node: Node) = {
    ParameterAbstractionDefinition(
      identifier = VersionedIdentifier.parse(node),
      parameters = (node \ "parameterAbstraction").map(n => ParameterAbstraction.parse(n))
    )
  }
}

case class ParameterAbstraction(logicalName: String,
                                format: String,
                                default: String,
                                provider: String,
                                required: Boolean,
                                usage: String,
                                permission: String)

object ParameterAbstraction {
  def parse(node: Node) = {
    val spiritUri = node.scope.getURI("spirit")
    val xilinxUri = node.scope.getURI("xilinx")
    ParameterAbstraction(
      logicalName = (node \@ s"{$xilinxUri}logicalName"),
      format = (node \@ s"{$spiritUri}format"),
      default = (node \@ s"{$xilinxUri}default"),
      provider = (node \@ s"{$xilinxUri}provider"),
      required = (node \ s"@{$xilinxUri}required").map(_.text.toBoolean).headOption.getOrElse(false),
      usage = (node \@ s"{$xilinxUri}usage"),
      permission = (node \@ s"{$xilinxUri}permission")
    )
  }
}

case class NativeAxiMMAbstraction() extends AbstractionDefinitionI {
  override val identifier: VersionedIdentifier = VersionedIdentifier("spinal", "interface", "aximm", "1.0")

  override def scalaInstance(origConfig: Map[String, String] = Map())(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[String] = {
    val busAbsDefOption = definitions.get(VersionedIdentifier("xilinx.com", "interface.param", "aximm", "1.0"))
    val config = (origConfig ++ busAbsDefOption.flatMap {
      case busAbsDef: ParameterAbstractionDefinition => Some(busAbsDef.parameters.map { p =>
        p.logicalName -> p.default
      }.filter(p => !origConfig.contains(p._1)).toMap)
      case _ => None
    }.getOrElse(Map.empty[String, String])).map { case (k, v) => k.toLowerCase -> v }
    def mapBool(v: String) = if (v == "1") "true" else "false"
    config("protocol").toLowerCase match {
      case "axi4" => {
        val mapped = config.flatMap { case (k, v) => k match {
          case "data_width" => Some("dataWidth" -> v)
          case "addr_width" => Some("addressWidth" -> v)
          case "id_width" => Some("idWidth" -> v)
          case "has_region" => Some("useRegion" -> mapBool(v))
          case "has_burst" => Some("useId" -> mapBool(v))
          case "has_lock" => Some("useLock" -> mapBool(v))
          case "has_prot" => Some("useProt" -> mapBool(v))
          case "has_cache" => Some("useCache" -> mapBool(v))
          case "has_qos" => Some("useQos" -> mapBool(v))
          case "has_wstrb" => Some("useStrb" -> mapBool(v))
          case "has_bresp" | "has_rresp" => Some("useResp" -> mapBool(v))
          case "aruser_width" => Some("arUserWidth" -> v)
          case "awuser_width" => Some("awUserWidth" -> v)
          case "ruser_width" => Some("rUserWidth" -> v)
          case "wuser_width" => Some("wUserWidth" -> v)
          case "buser_width" => Some("bUserWidth" -> v)
          case _ => None
        }} ++ Map(
          "useLen" -> "true"
        )
        val args = mapped.map { case (k, v) => s"$k = $v"}.mkString(", ")
        Some(s"Axi4(Axi4Config($args))")
      }
      case "axi3" => {
        Some("Axi3(Axi3Config())")
      }
      case _ => None
    }
  }

  override def scalaLibraries(config: Map[String, String])(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Seq[String] = Seq.empty
}

case class NativeAxisAbstraction() extends AbstractionDefinitionI {
  override val identifier: VersionedIdentifier = VersionedIdentifier("spinal", "interface", "axis", "1.0")

  override def scalaInstance(config: Map[String, String])(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[String] = {
    Some(s"Axi4Stream(Axi4StreamConfig())")
  }

  override def scalaLibraries(config: Map[String, String])(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Seq[String] = Seq.empty
}

case class GenericVectorAbstraction() extends AbstractionDefinitionI {
  override val identifier: VersionedIdentifier = VersionedIdentifier("spinal", "interface", "generic", "1.0")

  override def scalaInstance(config: Map[String, String])(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[String] = {
    val width = config.find { case (k, v) => k.toLowerCase == "portwidth" }.map(_._2).getOrElse(0)
    Some(s"Bits($width bit)")
  }

  override def scalaLibraries(config: Map[String, String])(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Seq[String] = Seq.empty
}