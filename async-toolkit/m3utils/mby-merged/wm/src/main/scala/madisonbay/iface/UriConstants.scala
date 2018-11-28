package madisonbay.iface

object UriConstants {

  case class UriParameter(key: String, value: Option[String])

  object UriParameter {
    val KeyPath     = "path"
  }

  object UriSegment {
    val Css         = "css"
    val Img         = "img"

    val Address     = "address"

    val RxPpe       = "rx_ppe"
    val Pipe        = "pipe"
    val Parser      = "parser"
    val Programmer  = "programmer"
    val Mapper      = "mapper"
  }

}
