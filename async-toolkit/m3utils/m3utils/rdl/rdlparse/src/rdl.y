%start root

root:
  list                     root_elem_list

root_elem_list:
  list                     root_elem root_elem_list
  empty

root_elem:
  component                component_def
  enum                     enum_def
  explicit_component       explicit_component_inst
  property_assign          property_assign
  property_def             property_definition

component_def:
  id_anon                  component_def_type T_mID '{' component_def_lst '}' opt_anonymous_inst_elems ';'
  noid_anon                component_def_type '{' component_def_lst '}' opt_anonymous_inst_elems ';'

opt_anonymous_inst_elems:
  yes                      anonymous_component_inst_elems
  no

component_def_lst:
  empty
  lst                      component_def_elem component_def_lst

component_def_elem:
  component_def            component_def
  component_inst           explicit_component_inst
  property_assign          property_assign
  enum_def                 enum_def

component_def_type:
  addrmap                  T_ADDRMAP
  regfile                  T_REGFILE
  reg                      T_REG
  field                    T_FIELD
  signal                   T_SIGNAL

enum_def:
  x                        T_ENUM T_mID enum_body ';'

explicit_component_inst:
  none                     T_mID component_inst_list ';'
  origin                   origin T_mID component_inst_list ';'
  alias                    alias_defn T_mID component_inst_list ';'
  origin_alias             origin alias_defn T_mID component_inst_list ';'

origin:
  internal                 T_INTERNAL
  external                 T_EXTERNAL

alias_defn:
  single                   T_ALIAS T_mID

component_inst_list:
  single                   component_inst_elem
  list                     component_inst_elem ',' component_inst_list

property_assign:
  defaultP                 default_property_assign ';'
  explicitP                explicit_property_assign ';'
  defTrueP                 property ';'
  postP                    post_property_assign ';'

property_definition:
  initial                  T_PROPERTY T_mID '{' property_body '}' ';'
  redefinition             T_PROPERTY T_mPROPERTY '{' property_body '}' ';'

id:
  id                       T_mID

property_body:
  pb0                      property_type property_usage opt_property_default
  pb1                      property_type property_default property_usage
  pb2                      property_usage property_type opt_property_default
  pb3                      property_usage property_default property_type
  pb4                      property_default property_type property_usage
  pb5                      property_default property_usage property_type

property_type:
  x                        T_TYPE T_EQ property_type_spec ';'

property_type_spec:
  string                   property_string_type 
  number                   property_number_type 
  boolean                  property_boolean_type
  ref                      property_ref_type 


property_usage:
  x                        T_COMPONENT T_EQ property_component_disjunction ';'

property_component_disjunction:
  single                   property_component
  list                     property_component '|' property_component_disjunction

property_default:
  str                      T_DEFAULT T_EQ str ';'
  num                      T_DEFAULT T_EQ num ';'
  true                     T_DEFAULT T_EQ T_TRUE ';'
  false                    T_DEFAULT T_EQ T_FALSE ';'

opt_property_default:
  x                        property_default
  empty

property_string_type:
  x                        T_STRING

property_number_type:
  x                        T_NUMBER

property_boolean_type:
  x                        T_BOOLEAN

property_ref_type:
  addrmap                  T_ADDRMAP
  reg                      T_REG
  regfile                  T_REGFILE
  field                    T_FIELD
  ref                      T_REF

str:
  x                        T_mSTR

num:
  x                        T_mNUM

property_component:
  signal                   T_SIGNAL
  addrmap                  T_ADDRMAP
  reg                      T_REG
  regfile                  T_REGFILE
  field                    T_FIELD
  all                      T_ALL

anonymous_component_inst_elems:
  x                        opt_external component_inst_elem_list

opt_external:
  yes                      T_EXTERNAL
  no

component_inst_elem_list:
  single                   component_inst_elem
  list                     component_inst_elem ',' component_inst_elem_list

component_inst_elem:
  x                        id opt_array opt_eq_num opt_at_num opt_inc_num opt_mod_num

opt_array:
  array                    array
  empty

opt_eq_num:
  eq_num                   T_EQ num
  empty

opt_at_num:
  at_num                   '@' num
  empty

opt_inc_num:
  inc_num                  T_mINC num
  empty

opt_mod_num:
  mod_num                  T_mMOD num
  empty

array:
  single                   '[' num ']'
  range                    '[' num ':' num ']'

instance_ref:
  list                    dotted_instance_ref_list
  listprop                dotted_instance_ref_list T_mDREF property

dotted_instance_ref_list:
  single                  instance_ref_elem
  list                    instance_ref_elem '.' dotted_instance_ref_list

instance_ref_elem:
  id                      id
  brack                   id '[' num ']' 

property:
  predefP                 predef_property
  userdefP                T_mPROPERTY

predef_property:
  name                    T_NAME           
  desc                    T_DESC           
  arbiter                 T_ARBITER        
  rset                    T_RSET           
  rclr                    T_RCLR           
  woclr                   T_WOCLR          
  woset                   T_WOSET          
  we                      T_WE             
  wel                     T_WEL            
  swwe                    T_SWWE           
  swwel                   T_SWWEL          
  hwset                   T_HWSET          
  hwclr                   T_HWCLR          
  swmod                   T_SWMOD          
  swacc                   T_SWACC          
  sticky                  T_STICKY         
  stickybit               T_STICKYBIT      
  intr                    T_INTR           
  anded                   T_ANDED          
  ored                    T_ORED           
  xored                   T_XORED          
  counter                 T_COUNTER        
  overflow                T_OVERFLOW       
  sharedextbus            T_SHAREDEXTBUS   
  errextbus               T_ERREXTBUS      
  reset                   T_RESET          
  littleendian            T_LITTLEENDIAN   
  bigendian               T_BIGENDIAN      
  rsvdset                 T_RSVDSET        
  rsvdsetX                T_RSVDSETX       
  bridge                  T_BRIDGE         
  shared                  T_SHARED         
  msb0                    T_MSB0           
  lsb0                    T_LSB0           
  sync                    T_SYNC           
  async                   T_ASYNC          
  cpuif_reset             T_CPUIF_RESET    
  field_reset             T_FIELD_RESET    
  activehigh              T_ACTIVEHIGH     
  activelow               T_ACTIVELOW      
  singlepulse             T_SINGLEPULSE    
  underflow               T_UNDERFLOW      
  incr                    T_INCR           
  decr                    T_DECR           
  incrwidth               T_INCRWIDTH      
  decrwidth               T_DECRWIDTH      
  incrvalue               T_INCRVALUE      
  decrvalue               T_DECRVALUE      
  saturate                T_SATURATE       
  decrsaturate            T_DECRSATURATE   
  threshold               T_THRESHOLD      
  decrthreshold           T_DECRTHRESHOLD  
  dontcompare             T_DONTCOMPARE    
  donttest                T_DONTTEST       
  internal                T_INTERNAL       
  alignment               T_ALIGNMENT      
  regwidth                T_REGWIDTH       
  fieldwidth              T_FIELDWIDTH     
  signalwidth             T_SIGNALWIDTH    
  accesswidth             T_ACCESSWIDTH    
  sw                      T_SW             
  hw                      T_HW             
  addressing              T_ADDRESSING     
  precedence              T_PRECEDENCE     
  encode                  T_ENCODE         
  resetsignal             T_RESETSIGNAL    
  clock                   T_CLOCK          
  mask                    T_MASK           
  enable                  T_ENABLE         
  hwenable                T_HWENABLE       
  hwmask                  T_HWMASK         
  haltmask                T_HALTMASK       
  haltenable              T_HALTENABLE     
  halt                    T_HALT           
  next                    T_NEXT           

default_property_assign:
  x                       T_DEFAULT explicit_property_assign

explicit_property_assign:
  mod                     property_modifier property
  eq                      property T_EQ property_assign_rhs

post_property_assign:
  x                       instance_ref T_EQ property_assign_rhs

property_modifier:
  posedge                 T_POSEDGE        
  negedge                 T_NEGEDGE        
  bothedge                T_BOTHEDGE       
  level                   T_LEVEL          
  nonsticky               T_NONSTICKY      

property_assign_rhs:
  const                   property_rvalue_constant
  enum                    T_ENUM enum_body
  iref                    instance_ref
  concat                  concat

property_rvalue_constant:
  num                     num
  str                     str
  keyword                 property_rvalue_keyword

property_rvalue_keyword:
  true                    T_TRUE           
  false                   T_FALSE          
  rw                      T_RW             
  wr                      T_WR             
  r                       T_R              
  w                       T_W              
  na                      T_NA             
  compact                 T_COMPACT        
  regalign                T_REGALIGN       
  fullalign               T_FULLALIGN      
  hw                      T_HW             
  sw                      T_SW             

enum_body:
  x                       '{' enum_entry_seq '}'

enum_entry_seq:
  empty
  list                    enum_entry enum_entry_seq

concat:
  x                       '{' concat_elem_list '}'

concat_elem_list:
  single                  concat_elem
  list                    concat_elem ',' concat_elem_list

concat_elem:
  iref                    instance_ref
  num                     num

enum_entry:
  x                       id T_EQ num opt_enum_property_assign_list ';'

opt_enum_property_assign_list:
  empty
  x                       '{' enum_property_assign_list '}

enum_property_assign_list:
  single                  enum_property_assign
  list                    enum_property_assign enum_property_assign_list

enum_property_assign:
  name                    T_NAME T_EQ str ';'  
  desc                    T_DESC T_EQ str ';'  

