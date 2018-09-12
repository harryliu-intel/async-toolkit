/** Provides classes and associated objects for building the model server
  *
  * ==Overview==
  * The "model-server" architecture was originally developed by the Highland Park SDK team in order to support their
  * verification activities. On Madison Bay, the approach is more generally used. All white-model users must implement
  * communication with the white model's 'server' protocol. An example C client is provided, and a Verilog DPI
  * interface is built on top of that model
  *
  * Upon started, the white model will generate a descriptor file, which a client must parse to determine
  * what port and machine to connect to.
  *
  * There are two categories of classes/objects in this package. The majority of these are derived from the
  * a Scheme-based description of the objects passed over the wire to implement the white-model client-server
  * interaction via over TCP sockets.
  *
  * ===Generated Classes===
  * The Scheme description of the structures is stored in the M3 code area (it relies on a custom Scheme interpreter
  * which integrates some M3 code)
  *
  * ===Human-Written Classes===
  * Other classes and objects are human-written to support these classes and provide elegant mechanisms
  * for using them.
 **/