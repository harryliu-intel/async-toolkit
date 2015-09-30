	begin_unit	 0
	declare_segment	 * -1 v.1
					-----FILE m3main.mc  -----
					-----LINE 1  -----
	import_global	 MM_AL 44 4 Struct 0 v.2
	import_global	 MI_AL 44 4 Struct 0 v.3
	import_global	 MI_ASCII 44 4 Struct 0 v.4
	import_global	 MM_ASCII 44 4 Struct 0 v.5
	import_global	 MI_Atom 44 4 Struct 0 v.6
	import_global	 MM_Atom 44 4 Struct 0 v.7
	import_global	 MM_AtomAtomTbl 44 4 Struct 0 v.8
	import_global	 MI_AtomAtomTbl 44 4 Struct 0 v.9
	import_global	 MI_AtomList 44 4 Struct 0 v.10
	import_global	 MM_AtomList 44 4 Struct 0 v.11
	import_global	 MI_BasicCtypes 44 4 Struct 0 v.12
	import_global	 MI_Boolean 44 4 Struct 0 v.13
	import_global	 MM_Boolean 44 4 Struct 0 v.14
	import_global	 MI_BreakHere 44 4 Struct 0 v.15
	import_global	 MM_BreakHere 44 4 Struct 0 v.16
	import_global	 MI_CConvert 44 4 Struct 0 v.17
	import_global	 MM_CConvert 44 4 Struct 0 v.18
	import_global	 MI_CTZ 44 4 Struct 0 v.19
	import_global	 MI_Cerrno 44 4 Struct 0 v.20
	import_global	 MI_ConnFD 44 4 Struct 0 v.21
	import_global	 MM_ConnRW 44 4 Struct 0 v.22
	import_global	 MI_ConnRW 44 4 Struct 0 v.23
	import_global	 MM_Convert 44 4 Struct 0 v.24
	import_global	 MI_Convert 44 4 Struct 0 v.25
	import_global	 MI_Csetjmp 44 4 Struct 0 v.26
	import_global	 MI_Cstddef 44 4 Struct 0 v.27
	import_global	 MI_Cstdlib 44 4 Struct 0 v.28
	import_global	 MI_Cstring 44 4 Struct 0 v.29
	import_global	 MI_Ctypes 44 4 Struct 0 v.30
	import_global	 MI_Date 44 4 Struct 0 v.31
	import_global	 MM_DateBsd 44 4 Struct 0 v.32
	import_global	 MI_Debug 44 4 Struct 0 v.33
	import_global	 MM_Debug 44 4 Struct 0 v.34
	import_global	 MI_DebugClass 44 4 Struct 0 v.35
	import_global	 MM_DebugClass 44 4 Struct 0 v.36
	import_global	 MM_DebugFmtPointer 44 4 Struct 0 v.37
	import_global	 MI_DebugStream 44 4 Struct 0 v.38
	import_global	 MI_DebugStreamList 44 4 Struct 0 v.39
	import_global	 MM_DebugStreamList 44 4 Struct 0 v.40
	import_global	 MI_DragonInt 44 4 Struct 0 v.41
	import_global	 MM_DragonInt 44 4 Struct 0 v.42
	import_global	 MI_DragonT 44 4 Struct 0 v.43
	import_global	 MM_DragonT 44 4 Struct 0 v.44
	import_global	 MI_Env 44 4 Struct 0 v.45
	import_global	 MM_Env 44 4 Struct 0 v.46
	import_global	 MM_Extended 44 4 Struct 0 v.47
	import_global	 MI_Extended 44 4 Struct 0 v.48
	import_global	 MM_ExtendedFloat 44 4 Struct 0 v.49
	import_global	 MI_ExtendedFloat 44 4 Struct 0 v.50
	import_global	 MI_FPU 44 4 Struct 0 v.51
	import_global	 MM_FPU 44 4 Struct 0 v.52
	import_global	 MI_FS 44 4 Struct 0 v.53
	import_global	 MM_FS 44 4 Struct 0 v.54
	import_global	 MM_FSPosix 44 4 Struct 0 v.55
	import_global	 MI_File 44 4 Struct 0 v.56
	import_global	 MM_FilePosix 44 4 Struct 0 v.57
	import_global	 MI_FilePosix 44 4 Struct 0 v.58
	import_global	 MM_FileRd 44 4 Struct 0 v.59
	import_global	 MI_FileRd 44 4 Struct 0 v.60
	import_global	 MM_FileWr 44 4 Struct 0 v.61
	import_global	 MI_FileWr 44 4 Struct 0 v.62
	import_global	 MI_FinDate 44 4 Struct 0 v.63
	import_global	 MM_FinDate 44 4 Struct 0 v.64
	import_global	 MM_Fingerprint 44 4 Struct 0 v.65
	import_global	 MI_Fingerprint 44 4 Struct 0 v.66
	import_global	 MM_FloatMode 44 4 Struct 0 v.67
	import_global	 MI_FloatMode 44 4 Struct 0 v.68
	import_global	 MI_Fmt 44 4 Struct 0 v.69
	import_global	 MM_Fmt 44 4 Struct 0 v.70
	import_global	 MM_FmtBuf 44 4 Struct 0 v.71
	import_global	 MI_FmtBuf 44 4 Struct 0 v.72
	import_global	 MI_FmtBufF 44 4 Struct 0 v.73
	import_global	 MI_FmtBufTest 44 4 Struct 0 v.74
	import_global	 MM_HMTime 44 4 Struct 0 v.75
	import_global	 MI_HMTime 44 4 Struct 0 v.76
	import_global	 MM_HMTimeToday 44 4 Struct 0 v.77
	import_global	 MI_Herrno 44 4 Struct 0 v.78
	import_global	 MI_IEEESpecial 44 4 Struct 0 v.79
	import_global	 MM_IEEESpecial 44 4 Struct 0 v.80
	import_global	 MM_IO 44 4 Struct 0 v.81
	import_global	 MI_IO 44 4 Struct 0 v.82
	import_global	 MI_IP 44 4 Struct 0 v.83
	import_global	 MM_IP 44 4 Struct 0 v.84
	import_global	 MI_Int32 44 4 Struct 0 v.85
	import_global	 MM_Int32 44 4 Struct 0 v.86
	import_global	 MI_IntFinDateTbl 44 4 Struct 0 v.87
	import_global	 MM_IntFinDateTbl 44 4 Struct 0 v.88
	import_global	 MM_IntList 44 4 Struct 0 v.89
	import_global	 MI_IntList 44 4 Struct 0 v.90
	import_global	 MI_Integer 44 4 Struct 0 v.91
	import_global	 MM_Integer 44 4 Struct 0 v.92
	import_global	 MM_Lex 44 4 Struct 0 v.93
	import_global	 MI_Lex 44 4 Struct 0 v.94
	import_global	 MI_LockedTextBooleanTbl 44 4 Struct 0 v.95
	import_global	 MM_LockedTextBooleanTbl 44 4 Struct 0 v.96
	import_global	 MM_LongFloat 44 4 Struct 0 v.97
	import_global	 MI_LongFloat 44 4 Struct 0 v.98
	import_global	 MM_LongReal 44 4 Struct 0 v.99
	import_global	 MI_LongReal 44 4 Struct 0 v.100
	import_global	 MI_LongRealRep 44 4 Struct 0 v.101
	import_global	 MI_LongrealList 44 4 Struct 0 v.102
	import_global	 MM_LongrealList 44 4 Struct 0 v.103
	import_global	 MM_LongrealType 44 4 Struct 0 v.104
	import_global	 MI_LongrealType 44 4 Struct 0 v.105
	import_global	 MI_M3_BUILTIN 44 4 Struct 0 v.106
	import_global	 MI_M3toC 44 4 Struct 0 v.107
	import_global	 MM_M3toC 44 4 Struct 0 v.108
	import_global	 MM_Main 44 4 Struct 0 v.109
	import_global	 MI_Main 44 4 Struct 0 v.110
	import_global	 MI_OSError 44 4 Struct 0 v.111
	import_global	 MI_OSErrorPosix 44 4 Struct 0 v.112
	import_global	 MM_OSErrorPosix 44 4 Struct 0 v.113
	import_global	 MM_Params 44 4 Struct 0 v.114
	import_global	 MI_Params 44 4 Struct 0 v.115
	import_global	 MM_ParseParams 44 4 Struct 0 v.116
	import_global	 MI_ParseParams 44 4 Struct 0 v.117
	import_global	 MI_Pathname 44 4 Struct 0 v.118
	import_global	 MM_PathnamePosix 44 4 Struct 0 v.119
	import_global	 MM_Pipe 44 4 Struct 0 v.120
	import_global	 MI_Pipe 44 4 Struct 0 v.121
	import_global	 MM_PipePosix 44 4 Struct 0 v.122
	import_global	 MI_Poly 44 4 Struct 0 v.123
	import_global	 MM_Poly 44 4 Struct 0 v.124
	import_global	 MI_PolyBasis 44 4 Struct 0 v.125
	import_global	 MM_PolyBasis 44 4 Struct 0 v.126
	import_global	 MI_Process 44 4 Struct 0 v.127
	import_global	 MM_ProcessPosix 44 4 Struct 0 v.128
	import_global	 MI_RT0 44 4 Struct 0 v.129
	import_global	 MM_RT0 44 4 Struct 0 v.130
	import_global	 MI_RT0u 44 4 Struct 0 v.131
	import_global	 MM_RT0u 44 4 Struct 0 v.132
	import_global	 MI_RTAllocator 44 4 Struct 0 v.133
	import_global	 MM_RTAllocator 44 4 Struct 0 v.134
	import_global	 MM_RTArgs 44 4 Struct 0 v.135
	import_global	 MI_RTArgs 44 4 Struct 0 v.136
	import_global	 MI_RTCollector 44 4 Struct 0 v.137
	import_global	 MM_RTCollector 44 4 Struct 0 v.138
	import_global	 MI_RTCollectorSRC 44 4 Struct 0 v.139
	import_global	 MI_RTExRep 44 4 Struct 0 v.140
	import_global	 MI_RTException 44 4 Struct 0 v.141
	import_global	 MM_RTException 44 4 Struct 0 v.142
	import_global	 MI_RTHeap 44 4 Struct 0 v.143
	import_global	 MM_RTHeap 44 4 Struct 0 v.144
	import_global	 MI_RTHeapDep 44 4 Struct 0 v.145
	import_global	 MM_RTHeapDep 44 4 Struct 0 v.146
	import_global	 MI_RTHeapEvent 44 4 Struct 0 v.147
	import_global	 MM_RTHeapInfo 44 4 Struct 0 v.148
	import_global	 MI_RTHeapInfo 44 4 Struct 0 v.149
	import_global	 MM_RTHeapMap 44 4 Struct 0 v.150
	import_global	 MI_RTHeapMap 44 4 Struct 0 v.151
	import_global	 MI_RTHeapRep 44 4 Struct 0 v.152
	import_global	 MM_RTHeapRep 44 4 Struct 0 v.153
	import_global	 MI_RTHooks 44 4 Struct 0 v.154
	import_global	 MM_RTHooks 44 4 Struct 0 v.155
	import_global	 MI_RTIO 44 4 Struct 0 v.156
	import_global	 MM_RTIO 44 4 Struct 0 v.157
	import_global	 MI_RTLinker 44 4 Struct 0 v.158
	import_global	 MM_RTLinker 44 4 Struct 0 v.159
	import_global	 MI_RTMachine 44 4 Struct 0 v.160
	import_global	 MM_RTMapOp 44 4 Struct 0 v.161
	import_global	 MI_RTMapOp 44 4 Struct 0 v.162
	import_global	 MI_RTMisc 44 4 Struct 0 v.163
	import_global	 MM_RTMisc 44 4 Struct 0 v.164
	import_global	 MM_RTModule 44 4 Struct 0 v.165
	import_global	 MI_RTModule 44 4 Struct 0 v.166
	import_global	 MM_RTOS 44 4 Struct 0 v.167
	import_global	 MI_RTOS 44 4 Struct 0 v.168
	import_global	 MM_RTParams 44 4 Struct 0 v.169
	import_global	 MI_RTParams 44 4 Struct 0 v.170
	import_global	 MM_RTPerfTool 44 4 Struct 0 v.171
	import_global	 MI_RTPerfTool 44 4 Struct 0 v.172
	import_global	 MI_RTProcedure 44 4 Struct 0 v.173
	import_global	 MM_RTProcedure 44 4 Struct 0 v.174
	import_global	 MI_RTProcedureSRC 44 4 Struct 0 v.175
	import_global	 MI_RTProcess 44 4 Struct 0 v.176
	import_global	 MM_RTProcess 44 4 Struct 0 v.177
	import_global	 MI_RTSignal 44 4 Struct 0 v.178
	import_global	 MM_RTSignal 44 4 Struct 0 v.179
	import_global	 MI_RTStack 44 4 Struct 0 v.180
	import_global	 MI_RTThread 44 4 Struct 0 v.181
	import_global	 MM_RTThread 44 4 Struct 0 v.182
	import_global	 MI_RTThreadInit 44 4 Struct 0 v.183
	import_global	 MM_RTThreadStk 44 4 Struct 0 v.184
	import_global	 MI_RTType 44 4 Struct 0 v.185
	import_global	 MM_RTType 44 4 Struct 0 v.186
	import_global	 MM_RTTypeMap 44 4 Struct 0 v.187
	import_global	 MI_RTTypeMap 44 4 Struct 0 v.188
	import_global	 MI_RTTypeSRC 44 4 Struct 0 v.189
	import_global	 MI_RTWeakRef 44 4 Struct 0 v.190
	import_global	 MI_Rd 44 4 Struct 0 v.191
	import_global	 MI_RdClass 44 4 Struct 0 v.192
	import_global	 MM_RdCopy 44 4 Struct 0 v.193
	import_global	 MI_RdCopy 44 4 Struct 0 v.194
	import_global	 MM_RdImpl 44 4 Struct 0 v.195
	import_global	 MM_RdMove 44 4 Struct 0 v.196
	import_global	 MM_RdWrPipe 44 4 Struct 0 v.197
	import_global	 MI_RdWrPipe 44 4 Struct 0 v.198
	import_global	 MM_RdWrReset 44 4 Struct 0 v.199
	import_global	 MI_RdWrReset 44 4 Struct 0 v.200
	import_global	 MI_Real 44 4 Struct 0 v.201
	import_global	 MM_Real 44 4 Struct 0 v.202
	import_global	 MM_RealFloat 44 4 Struct 0 v.203
	import_global	 MI_RealFloat 44 4 Struct 0 v.204
	import_global	 MI_RealRep 44 4 Struct 0 v.205
	import_global	 MM_RefSeq 44 4 Struct 0 v.206
	import_global	 MI_RefSeq 44 4 Struct 0 v.207
	import_global	 MI_RefSeqRep 44 4 Struct 0 v.208
	import_global	 MI_Refany 44 4 Struct 0 v.209
	import_global	 MM_Refany 44 4 Struct 0 v.210
	import_global	 MI_RegularFile 44 4 Struct 0 v.211
	import_global	 MM_RegularFile 44 4 Struct 0 v.212
	import_global	 MM_SafeTZ 44 4 Struct 0 v.213
	import_global	 MM_Scan 44 4 Struct 0 v.214
	import_global	 MI_Scan 44 4 Struct 0 v.215
	import_global	 MI_ScanList 44 4 Struct 0 v.216
	import_global	 MM_ScanList 44 4 Struct 0 v.217
	import_global	 MI_Scheduler 44 4 Struct 0 v.218
	import_global	 MI_SchedulerIndirection 44 4 Struct 0 v.219
	import_global	 MM_SchedulerIndirectionPM3 44 4 Struct 0 v.220
	import_global	 MI_SchedulerPosix 44 4 Struct 0 v.221
	import_global	 MI_Stdio 44 4 Struct 0 v.222
	import_global	 MM_Stdio 44 4 Struct 0 v.223
	import_global	 MM_TCP 44 4 Struct 0 v.224
	import_global	 MI_TCP 44 4 Struct 0 v.225
	import_global	 MI_TCPHack 44 4 Struct 0 v.226
	import_global	 MM_TCPHackNull 44 4 Struct 0 v.227
	import_global	 MI_TCPPosix 44 4 Struct 0 v.228
	import_global	 MI_TCPSpecial 44 4 Struct 0 v.229
	import_global	 MM_TZ 44 4 Struct 0 v.230
	import_global	 MI_TZ 44 4 Struct 0 v.231
	import_global	 MI_Terminal 44 4 Struct 0 v.232
	import_global	 MM_Terminal 44 4 Struct 0 v.233
	import_global	 MI_Text 44 4 Struct 0 v.234
	import_global	 MM_Text 44 4 Struct 0 v.235
	import_global	 MI_TextBooleanTbl 44 4 Struct 0 v.236
	import_global	 MM_TextBooleanTbl 44 4 Struct 0 v.237
	import_global	 MI_TextF 44 4 Struct 0 v.238
	import_global	 MM_TextList 44 4 Struct 0 v.239
	import_global	 MI_TextList 44 4 Struct 0 v.240
	import_global	 MI_TextRd 44 4 Struct 0 v.241
	import_global	 MM_TextRd 44 4 Struct 0 v.242
	import_global	 MM_TextReader 44 4 Struct 0 v.243
	import_global	 MI_TextReader 44 4 Struct 0 v.244
	import_global	 MM_TextRefTbl 44 4 Struct 0 v.245
	import_global	 MI_TextRefTbl 44 4 Struct 0 v.246
	import_global	 MM_TextSeq 44 4 Struct 0 v.247
	import_global	 MI_TextSeq 44 4 Struct 0 v.248
	import_global	 MI_TextSeqRep 44 4 Struct 0 v.249
	import_global	 MM_TextSet 44 4 Struct 0 v.250
	import_global	 MI_TextSet 44 4 Struct 0 v.251
	import_global	 MM_TextSetDef 44 4 Struct 0 v.252
	import_global	 MI_TextSetDef 44 4 Struct 0 v.253
	import_global	 MM_TextUtils 44 4 Struct 0 v.254
	import_global	 MI_TextUtils 44 4 Struct 0 v.255
	import_global	 MM_TextUtilsFmt 44 4 Struct 0 v.256
	import_global	 MM_TextWr 44 4 Struct 0 v.257
	import_global	 MI_TextWr 44 4 Struct 0 v.258
	import_global	 MI_Thread 44 4 Struct 0 v.259
	import_global	 MI_ThreadEvent 44 4 Struct 0 v.260
	import_global	 MI_ThreadF 44 4 Struct 0 v.261
	import_global	 MM_ThreadPosix 44 4 Struct 0 v.262
	import_global	 MI_Time 44 4 Struct 0 v.263
	import_global	 MM_TimePosix 44 4 Struct 0 v.264
	import_global	 MI_TimePosix 44 4 Struct 0 v.265
	import_global	 MI_Udir 44 4 Struct 0 v.266
	import_global	 MI_Uerror 44 4 Struct 0 v.267
	import_global	 MM_Uerror 44 4 Struct 0 v.268
	import_global	 MI_Uexec 44 4 Struct 0 v.269
	import_global	 MM_Uin 44 4 Struct 0 v.270
	import_global	 MI_Uin 44 4 Struct 0 v.271
	import_global	 MI_Umman 44 4 Struct 0 v.272
	import_global	 MI_Unetdb 44 4 Struct 0 v.273
	import_global	 MM_Unetdb 44 4 Struct 0 v.274
	import_global	 MI_Unix 44 4 Struct 0 v.275
	import_global	 MM_UnsafeHash 44 4 Struct 0 v.276
	import_global	 MI_UnsafeRd 44 4 Struct 0 v.277
	import_global	 MI_UnsafeWr 44 4 Struct 0 v.278
	import_global	 MI_Uprocess 44 4 Struct 0 v.279
	import_global	 MI_Uresource 44 4 Struct 0 v.280
	import_global	 MM_Usignal 44 4 Struct 0 v.281
	import_global	 MI_Usignal 44 4 Struct 0 v.282
	import_global	 MI_Usocket 44 4 Struct 0 v.283
	import_global	 MI_Ustat 44 4 Struct 0 v.284
	import_global	 MI_Utime 44 4 Struct 0 v.285
	import_global	 MM_UtimeOpsC 44 4 Struct 0 v.286
	import_global	 MI_UtimeOpsC 44 4 Struct 0 v.287
	import_global	 MI_UtimeWrap 44 4 Struct 0 v.288
	import_global	 MM_UtimeWrap 44 4 Struct 0 v.289
	import_global	 MI_Utypes 44 4 Struct 0 v.290
	import_global	 MM_Utypes 44 4 Struct 0 v.291
	import_global	 MI_Uugid 44 4 Struct 0 v.292
	import_global	 MI_Uuio 44 4 Struct 0 v.293
	import_global	 MM_Word 44 4 Struct 0 v.294
	import_global	 MI_Word 44 4 Struct 0 v.295
	import_global	 MI_Wr 44 4 Struct 0 v.296
	import_global	 MI_WrClass 44 4 Struct 0 v.297
	import_global	 MM_WrMove 44 4 Struct 0 v.298
	import_global	 MM_WrPosix 44 4 Struct 0 v.299
	import_global	 MI_Wx 44 4 Struct 0 v.300
	import_global	 MM_Wx 44 4 Struct 0 v.301
	import_global	 MM_XTime 44 4 Struct 0 v.302
	import_global	 MI_XTime 44 4 Struct 0 v.303
	declare_global	 modules 1208 4 Struct 0 F F v.304
	declare_global	 m3_link_info 32 4 Struct 0 F F v.305
	declare_procedure	 main 3 Int 0 0 T * p.1
	declare_param	 argc 4 4 Int 0 F F 1 v.306
	declare_param	 argv 4 4 Addr 0 F F 1 v.307
	declare_param	 envp 4 4 Int 0 F F 1 v.308
	begin_procedure	 p.1
	declare_temp	 4 4 Addr T v.309
	load_address	 v.305 0
	store		 v.158 148 Addr
	load		 v.306 0 Int
	store		 v.305 8 Int
	load		 v.307 0 Addr
	store		 v.305 12 Addr
	load		 v.308 0 Addr
	store		 v.305 16 Addr
	load_integer	 0
	store		 v.305 20 Int
	load_address	 v.143 0
	store		 v.304 0 Addr
	load_address	 v.149 0
	store		 v.304 4 Addr
	load_address	 v.279 0
	store		 v.304 8 Addr
	load_address	 v.178 0
	store		 v.304 12 Addr
	load_address	 v.133 0
	store		 v.304 16 Addr
	load_address	 v.265 0
	store		 v.304 20 Addr
	load_address	 v.263 0
	store		 v.304 24 Addr
	load_address	 v.260 0
	store		 v.304 28 Addr
	load_address	 v.181 0
	store		 v.304 32 Addr
	load_address	 v.20 0
	store		 v.304 36 Addr
	load_address	 v.183 0
	store		 v.304 40 Addr
	load_address	 v.221 0
	store		 v.304 44 Addr
	load_address	 v.218 0
	store		 v.304 48 Addr
	load_address	 v.282 0
	store		 v.304 52 Addr
	load_address	 v.272 0
	store		 v.304 56 Addr
	load_address	 v.125 0
	store		 v.304 60 Addr
	load_address	 v.123 0
	store		 v.304 64 Addr
	load_address	 v.66 0
	store		 v.304 68 Addr
	load_address	 v.173 0
	store		 v.304 72 Addr
	load_address	 v.175 0
	store		 v.304 76 Addr
	load_address	 v.180 0
	store		 v.304 80 Addr
	load_address	 v.140 0
	store		 v.304 84 Addr
	load_address	 v.141 0
	store		 v.304 88 Addr
	load_address	 v.176 0
	store		 v.304 92 Addr
	load_address	 v.280 0
	store		 v.304 96 Addr
	load_address	 v.269 0
	store		 v.304 100 Addr
	load_address	 v.172 0
	store		 v.304 104 Addr
	load_address	 v.136 0
	store		 v.304 108 Addr
	load_address	 v.170 0
	store		 v.304 112 Addr
	load_address	 v.188 0
	store		 v.304 116 Addr
	load_address	 v.162 0
	store		 v.304 120 Addr
	load_address	 v.293 0
	store		 v.304 124 Addr
	load_address	 v.285 0
	store		 v.304 128 Addr
	load_address	 v.290 0
	store		 v.304 132 Addr
	load_address	 v.275 0
	store		 v.304 136 Addr
	load_address	 v.168 0
	store		 v.304 140 Addr
	load_address	 v.156 0
	store		 v.304 144 Addr
	load_address	 v.234 0
	store		 v.304 148 Addr
	load_address	 v.238 0
	store		 v.304 152 Addr
	load_address	 v.27 0
	store		 v.304 156 Addr
	load_address	 v.28 0
	store		 v.304 160 Addr
	load_address	 v.107 0
	store		 v.304 164 Addr
	load_address	 v.166 0
	store		 v.304 168 Addr
	load_address	 v.189 0
	store		 v.304 172 Addr
	load_address	 v.185 0
	store		 v.304 176 Addr
	load_address	 v.151 0
	store		 v.304 180 Addr
	load_address	 v.147 0
	store		 v.304 184 Addr
	load_address	 v.190 0
	store		 v.304 188 Addr
	load_address	 v.137 0
	store		 v.304 192 Addr
	load_address	 v.139 0
	store		 v.304 196 Addr
	load_address	 v.131 0
	store		 v.304 200 Addr
	load_address	 v.26 0
	store		 v.304 204 Addr
	load_address	 v.160 0
	store		 v.304 208 Addr
	load_address	 v.145 0
	store		 v.304 212 Addr
	load_address	 v.129 0
	store		 v.304 216 Addr
	load_address	 v.152 0
	store		 v.304 220 Addr
	load_address	 v.295 0
	store		 v.304 224 Addr
	load_address	 v.12 0
	store		 v.304 228 Addr
	load_address	 v.30 0
	store		 v.304 232 Addr
	load_address	 v.29 0
	store		 v.304 236 Addr
	load_address	 v.163 0
	store		 v.304 240 Addr
	load_address	 v.68 0
	store		 v.304 244 Addr
	load_address	 v.261 0
	store		 v.304 248 Addr
	load_address	 v.259 0
	store		 v.304 252 Addr
	load_address	 v.154 0
	store		 v.304 256 Addr
	load_address	 v.158 0
	store		 v.304 260 Addr
	load_address	 v.106 0
	store		 v.304 264 Addr
	load_address	 v.144 0
	store		 v.304 268 Addr
	load_address	 v.148 0
	store		 v.304 272 Addr
	load_address	 v.179 0
	store		 v.304 276 Addr
	load_address	 v.159 0
	store		 v.304 280 Addr
	load_address	 v.134 0
	store		 v.304 284 Addr
	load_address	 v.155 0
	store		 v.304 288 Addr
	load_address	 v.264 0
	store		 v.304 292 Addr
	load_address	 v.182 0
	store		 v.304 296 Addr
	load_address	 v.184 0
	store		 v.304 300 Addr
	load_address	 v.153 0
	store		 v.304 304 Addr
	load_address	 v.281 0
	store		 v.304 308 Addr
	load_address	 v.174 0
	store		 v.304 312 Addr
	load_address	 v.126 0
	store		 v.304 316 Addr
	load_address	 v.124 0
	store		 v.304 320 Addr
	load_address	 v.65 0
	store		 v.304 324 Addr
	load_address	 v.142 0
	store		 v.304 328 Addr
	load_address	 v.177 0
	store		 v.304 332 Addr
	load_address	 v.171 0
	store		 v.304 336 Addr
	load_address	 v.135 0
	store		 v.304 340 Addr
	load_address	 v.169 0
	store		 v.304 344 Addr
	load_address	 v.161 0
	store		 v.304 348 Addr
	load_address	 v.187 0
	store		 v.304 352 Addr
	load_address	 v.291 0
	store		 v.304 356 Addr
	load_address	 v.167 0
	store		 v.304 360 Addr
	load_address	 v.157 0
	store		 v.304 364 Addr
	load_address	 v.235 0
	store		 v.304 368 Addr
	load_address	 v.276 0
	store		 v.304 372 Addr
	load_address	 v.108 0
	store		 v.304 376 Addr
	load_address	 v.165 0
	store		 v.304 380 Addr
	load_address	 v.186 0
	store		 v.304 384 Addr
	load_address	 v.150 0
	store		 v.304 388 Addr
	load_address	 v.138 0
	store		 v.304 392 Addr
	load_address	 v.132 0
	store		 v.304 396 Addr
	load_address	 v.146 0
	store		 v.304 400 Addr
	load_address	 v.130 0
	store		 v.304 404 Addr
	load_address	 v.294 0
	store		 v.304 408 Addr
	load_address	 v.164 0
	store		 v.304 412 Addr
	load_address	 v.67 0
	store		 v.304 416 Addr
	load_address	 v.262 0
	store		 v.304 420 Addr
	load_address	 v.9 0
	store		 v.304 424 Addr
	load_address	 v.6 0
	store		 v.304 428 Addr
	load_address	 v.8 0
	store		 v.304 432 Addr
	load_address	 v.7 0
	store		 v.304 436 Addr
	load_address	 v.10 0
	store		 v.304 440 Addr
	load_address	 v.11 0
	store		 v.304 444 Addr
	load_address	 v.17 0
	store		 v.304 448 Addr
	load_address	 v.18 0
	store		 v.304 452 Addr
	load_address	 v.25 0
	store		 v.304 456 Addr
	load_address	 v.24 0
	store		 v.304 460 Addr
	load_address	 v.278 0
	store		 v.304 464 Addr
	load_address	 v.297 0
	store		 v.304 468 Addr
	load_address	 v.296 0
	store		 v.304 472 Addr
	load_address	 v.298 0
	store		 v.304 476 Addr
	load_address	 v.299 0
	store		 v.304 480 Addr
	load_address	 v.201 0
	store		 v.304 484 Addr
	load_address	 v.202 0
	store		 v.304 488 Addr
	load_address	 v.100 0
	store		 v.304 492 Addr
	load_address	 v.99 0
	store		 v.304 496 Addr
	load_address	 v.48 0
	store		 v.304 500 Addr
	load_address	 v.47 0
	store		 v.304 504 Addr
	load_address	 v.205 0
	store		 v.304 508 Addr
	load_address	 v.41 0
	store		 v.304 512 Addr
	load_address	 v.42 0
	store		 v.304 516 Addr
	load_address	 v.43 0
	store		 v.304 520 Addr
	load_address	 v.44 0
	store		 v.304 524 Addr
	load_address	 v.51 0
	store		 v.304 528 Addr
	load_address	 v.52 0
	store		 v.304 532 Addr
	load_address	 v.204 0
	store		 v.304 536 Addr
	load_address	 v.203 0
	store		 v.304 540 Addr
	load_address	 v.101 0
	store		 v.304 544 Addr
	load_address	 v.98 0
	store		 v.304 548 Addr
	load_address	 v.97 0
	store		 v.304 552 Addr
	load_address	 v.50 0
	store		 v.304 556 Addr
	load_address	 v.49 0
	store		 v.304 560 Addr
	load_address	 v.267 0
	store		 v.304 564 Addr
	load_address	 v.268 0
	store		 v.304 568 Addr
	load_address	 v.249 0
	store		 v.304 572 Addr
	load_address	 v.248 0
	store		 v.304 576 Addr
	load_address	 v.247 0
	store		 v.304 580 Addr
	load_address	 v.118 0
	store		 v.304 584 Addr
	load_address	 v.119 0
	store		 v.304 588 Addr
	load_address	 v.45 0
	store		 v.304 592 Addr
	load_address	 v.46 0
	store		 v.304 596 Addr
	load_address	 v.284 0
	store		 v.304 600 Addr
	load_address	 v.266 0
	store		 v.304 604 Addr
	load_address	 v.292 0
	store		 v.304 608 Addr
	load_address	 v.232 0
	store		 v.304 612 Addr
	load_address	 v.211 0
	store		 v.304 616 Addr
	load_address	 v.53 0
	store		 v.304 620 Addr
	load_address	 v.121 0
	store		 v.304 624 Addr
	load_address	 v.58 0
	store		 v.304 628 Addr
	load_address	 v.112 0
	store		 v.304 632 Addr
	load_address	 v.111 0
	store		 v.304 636 Addr
	load_address	 v.56 0
	store		 v.304 640 Addr
	load_address	 v.127 0
	store		 v.304 644 Addr
	load_address	 v.74 0
	store		 v.304 648 Addr
	load_address	 v.73 0
	store		 v.304 652 Addr
	load_address	 v.72 0
	store		 v.304 656 Addr
	load_address	 v.69 0
	store		 v.304 660 Addr
	load_address	 v.233 0
	store		 v.304 664 Addr
	load_address	 v.212 0
	store		 v.304 668 Addr
	load_address	 v.54 0
	store		 v.304 672 Addr
	load_address	 v.55 0
	store		 v.304 676 Addr
	load_address	 v.57 0
	store		 v.304 680 Addr
	load_address	 v.120 0
	store		 v.304 684 Addr
	load_address	 v.122 0
	store		 v.304 688 Addr
	load_address	 v.128 0
	store		 v.304 692 Addr
	load_address	 v.113 0
	store		 v.304 696 Addr
	load_address	 v.71 0
	store		 v.304 700 Addr
	load_address	 v.70 0
	store		 v.304 704 Addr
	load_address	 v.277 0
	store		 v.304 708 Addr
	load_address	 v.192 0
	store		 v.304 712 Addr
	load_address	 v.191 0
	store		 v.304 716 Addr
	load_address	 v.195 0
	store		 v.304 720 Addr
	load_address	 v.196 0
	store		 v.304 724 Addr
	load_address	 v.79 0
	store		 v.304 728 Addr
	load_address	 v.80 0
	store		 v.304 732 Addr
	load_address	 v.94 0
	store		 v.304 736 Addr
	load_address	 v.93 0
	store		 v.304 740 Addr
	load_address	 v.241 0
	store		 v.304 744 Addr
	load_address	 v.242 0
	store		 v.304 748 Addr
	load_address	 v.215 0
	store		 v.304 752 Addr
	load_address	 v.214 0
	store		 v.304 756 Addr
	load_address	 v.115 0
	store		 v.304 760 Addr
	load_address	 v.114 0
	store		 v.304 764 Addr
	load_address	 v.117 0
	store		 v.304 768 Addr
	load_address	 v.116 0
	store		 v.304 772 Addr
	load_address	 v.60 0
	store		 v.304 776 Addr
	load_address	 v.59 0
	store		 v.304 780 Addr
	load_address	 v.62 0
	store		 v.304 784 Addr
	load_address	 v.61 0
	store		 v.304 788 Addr
	load_address	 v.222 0
	store		 v.304 792 Addr
	load_address	 v.223 0
	store		 v.304 796 Addr
	load_address	 v.209 0
	store		 v.304 800 Addr
	load_address	 v.210 0
	store		 v.304 804 Addr
	load_address	 v.208 0
	store		 v.304 808 Addr
	load_address	 v.207 0
	store		 v.304 812 Addr
	load_address	 v.206 0
	store		 v.304 816 Addr
	load_address	 v.38 0
	store		 v.304 820 Addr
	load_address	 v.39 0
	store		 v.304 824 Addr
	load_address	 v.40 0
	store		 v.304 828 Addr
	load_address	 v.82 0
	store		 v.304 832 Addr
	load_address	 v.81 0
	store		 v.304 836 Addr
	load_address	 v.251 0
	store		 v.304 840 Addr
	load_address	 v.250 0
	store		 v.304 844 Addr
	load_address	 v.246 0
	store		 v.304 848 Addr
	load_address	 v.245 0
	store		 v.304 852 Addr
	load_address	 v.253 0
	store		 v.304 856 Addr
	load_address	 v.252 0
	store		 v.304 860 Addr
	load_address	 v.15 0
	store		 v.304 864 Addr
	load_address	 v.16 0
	store		 v.304 868 Addr
	load_address	 v.258 0
	store		 v.304 872 Addr
	load_address	 v.257 0
	store		 v.304 876 Addr
	load_address	 v.13 0
	store		 v.304 880 Addr
	load_address	 v.14 0
	store		 v.304 884 Addr
	load_address	 v.236 0
	store		 v.304 888 Addr
	load_address	 v.237 0
	store		 v.304 892 Addr
	load_address	 v.95 0
	store		 v.304 896 Addr
	load_address	 v.96 0
	store		 v.304 900 Addr
	load_address	 v.200 0
	store		 v.304 904 Addr
	load_address	 v.199 0
	store		 v.304 908 Addr
	load_address	 v.31 0
	store		 v.304 912 Addr
	load_address	 v.32 0
	store		 v.304 916 Addr
	load_address	 v.287 0
	store		 v.304 920 Addr
	load_address	 v.286 0
	store		 v.304 924 Addr
	load_address	 v.240 0
	store		 v.304 928 Addr
	load_address	 v.239 0
	store		 v.304 932 Addr
	load_address	 v.194 0
	store		 v.304 936 Addr
	load_address	 v.193 0
	store		 v.304 940 Addr
	load_address	 v.244 0
	store		 v.304 944 Addr
	load_address	 v.243 0
	store		 v.304 948 Addr
	load_address	 v.78 0
	store		 v.304 952 Addr
	load_address	 v.273 0
	store		 v.304 956 Addr
	load_address	 v.274 0
	store		 v.304 960 Addr
	load_address	 v.283 0
	store		 v.304 964 Addr
	load_address	 v.21 0
	store		 v.304 968 Addr
	load_address	 v.226 0
	store		 v.304 972 Addr
	load_address	 v.227 0
	store		 v.304 976 Addr
	load_address	 v.228 0
	store		 v.304 980 Addr
	load_address	 v.229 0
	store		 v.304 984 Addr
	load_address	 v.225 0
	store		 v.304 988 Addr
	load_address	 v.271 0
	store		 v.304 992 Addr
	load_address	 v.83 0
	store		 v.304 996 Addr
	load_address	 v.224 0
	store		 v.304 1000 Addr
	load_address	 v.270 0
	store		 v.304 1004 Addr
	load_address	 v.84 0
	store		 v.304 1008 Addr
	load_address	 v.3 0
	store		 v.304 1012 Addr
	load_address	 v.2 0
	store		 v.304 1016 Addr
	load_address	 v.23 0
	store		 v.304 1020 Addr
	load_address	 v.22 0
	store		 v.304 1024 Addr
	load_address	 v.303 0
	store		 v.304 1028 Addr
	load_address	 v.302 0
	store		 v.304 1032 Addr
	load_address	 v.19 0
	store		 v.304 1036 Addr
	load_address	 v.219 0
	store		 v.304 1040 Addr
	load_address	 v.220 0
	store		 v.304 1044 Addr
	load_address	 v.288 0
	store		 v.304 1048 Addr
	load_address	 v.289 0
	store		 v.304 1052 Addr
	load_address	 v.91 0
	store		 v.304 1056 Addr
	load_address	 v.92 0
	store		 v.304 1060 Addr
	load_address	 v.87 0
	store		 v.304 1064 Addr
	load_address	 v.63 0
	store		 v.304 1068 Addr
	load_address	 v.88 0
	store		 v.304 1072 Addr
	load_address	 v.64 0
	store		 v.304 1076 Addr
	load_address	 v.90 0
	store		 v.304 1080 Addr
	load_address	 v.89 0
	store		 v.304 1084 Addr
	load_address	 v.85 0
	store		 v.304 1088 Addr
	load_address	 v.86 0
	store		 v.304 1092 Addr
	load_address	 v.105 0
	store		 v.304 1096 Addr
	load_address	 v.104 0
	store		 v.304 1100 Addr
	load_address	 v.102 0
	store		 v.304 1104 Addr
	load_address	 v.103 0
	store		 v.304 1108 Addr
	load_address	 v.216 0
	store		 v.304 1112 Addr
	load_address	 v.217 0
	store		 v.304 1116 Addr
	load_address	 v.4 0
	store		 v.304 1120 Addr
	load_address	 v.5 0
	store		 v.304 1124 Addr
	load_address	 v.300 0
	store		 v.304 1128 Addr
	load_address	 v.301 0
	store		 v.304 1132 Addr
	load_address	 v.255 0
	store		 v.304 1136 Addr
	load_address	 v.254 0
	store		 v.304 1140 Addr
	load_address	 v.256 0
	store		 v.304 1144 Addr
	load_address	 v.76 0
	store		 v.304 1148 Addr
	load_address	 v.231 0
	store		 v.304 1152 Addr
	load_address	 v.35 0
	store		 v.304 1156 Addr
	load_address	 v.33 0
	store		 v.304 1160 Addr
	load_address	 v.75 0
	store		 v.304 1164 Addr
	load_address	 v.77 0
	store		 v.304 1168 Addr
	load_address	 v.213 0
	store		 v.304 1172 Addr
	load_address	 v.230 0
	store		 v.304 1176 Addr
	load_address	 v.36 0
	store		 v.304 1180 Addr
	load_address	 v.34 0
	store		 v.304 1184 Addr
	load_address	 v.37 0
	store		 v.304 1188 Addr
	load_address	 v.198 0
	store		 v.304 1192 Addr
	load_address	 v.197 0
	store		 v.304 1196 Addr
	load_address	 v.110 0
	store		 v.304 1200 Addr
	load_address	 v.109 0
	store		 v.304 1204 Addr
	load_integer	 302
	store		 v.305 0 Int
	load_address	 v.304 0
	store		 v.305 4 Addr
	load_integer	 4194304
	store		 v.305 28 Addr
	load_address	 v.309 0
	store		 v.305 24 Addr
	start_call_indirect	 Void 0
	load		 v.159 40 Addr
	call_indirect	 Void 0
	exit_proc	 Void
	free_temp	 v.309
	end_procedure	 p.1
	end_unit	
