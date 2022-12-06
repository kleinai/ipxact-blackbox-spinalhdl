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

  override def scalaInstance(origConfig: Map[String, String] = Map())(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[(String, Seq[String])] = {
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
        Some((s"Axi4(Axi4Config($args))",
          Seq(
            "spinal.lib.bus.amba4.axi.Axi4",
            "spinal.lib.bus.amba4.axi.Axi4Config"
          )
        ))
      }
      case "axi3" => ???
      case _ => None
    }
  }

  override def scalaDefinition(tabDepth: Int)(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[(String, Seq[String])] = None
}

case class NativeAxisAbstraction() extends AbstractionDefinitionI {
  override val identifier: VersionedIdentifier = VersionedIdentifier("spinal", "interface", "axis", "1.0")

  override def scalaInstance(origConfig: Map[String, String])(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[(String, Seq[String])] = {
    val busAbsDefOption = definitions.get(VersionedIdentifier("xilinx.com", "interface.param", "axis", "1.0"))
    val config = (origConfig ++ busAbsDefOption.flatMap {
      case busAbsDef: ParameterAbstractionDefinition => Some(busAbsDef.parameters.map { p =>
        p.logicalName -> p.default
      }.filter(p => !origConfig.contains(p._1)).toMap)
      case _ => None
    }.getOrElse(Map.empty[String, String])).map { case (k, v) => k.toLowerCase -> v }

    def mapBool(v: String) = if (v == "1") "true" else "false"

    val mapped = config.flatMap { case (k, v) => k match {
      case "tdata_num_bytes" => Some("dataWidth" -> v)
      case "tdest_width" => Some("destWidth" -> v)
      case "tid_width" => Some("idWidth" -> v)
      case "tuser_width" => Some("userWidth" -> v)
      case "has_tready" => if (v != "1") throw new Exception("HAS_TREADY=0 is not yet supported in native Axi4Stream translations") else None
      case "has_tstrb" => Some("useStrb", mapBool(v))
      case "has_tkeep" => Some("useKeep", mapBool(v))
      case "has_tlast" => Some("useLast", mapBool(v))
      case _ => None
    }} ++ Map(
      "useId" -> "true",
      "useDest" -> "true",
      "useUser" -> "true"
    )
    val args = mapped.map { case (k, v) => s"$k = $v" }.mkString(", ")
    Some((s"Axi4Stream(Axi4StreamConfig($args))",
      Seq(
        "spinal.lib.bus.amba4.axis.Axi4Stream",
        "spinal.lib.bus.amba4.axis.Axi4StreamConfig"
      )
    ))
  }

  override def scalaDefinition(tabDepth: Int)(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[(String, Seq[String])] = None
}

case class GenericVectorAbstraction() extends AbstractionDefinitionI {
  override val identifier: VersionedIdentifier = VersionedIdentifier("spinal", "interface", "generic", "1.0")

  override def scalaInstance(config: Map[String, String])(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[(String, Seq[String])] = {
    val width = config.find { case (k, v) => k.toLowerCase == "portwidth" }.map(_._2).getOrElse(0)
    Some((s"Bits($width bit)",
      Seq(
        "spinal.core.Bits"
      )
    ))
  }

  override def scalaDefinition(tabDepth: Int)(implicit definitions: AbstractMap[VersionedIdentifier, Any]): Option[(String, Seq[String])] = None
}