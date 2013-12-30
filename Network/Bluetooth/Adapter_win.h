typedef ULONGLONG AddrBTH;

typedef struct {
  USHORT   addressFamily;
  AddrBTH  btAddr;
  GUID     serviceClassId;
  ULONG    port;
} SockAddrBTH;
