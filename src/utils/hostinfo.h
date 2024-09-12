#if !defined(HOSTINFO_H) && !LLVM_AVAILABLE
#define HOSTINFO_H 

void hostinfo_x86_features(X86Features *cpu_features);
ArchType hostinfo_arch_type(void);
EnvironmentType hostinfo_env_type(void);
OsType hostinfo_os_type(void);
VendorType hostinfo_vendor_type(void);
const char * hostinfo_default_triple(void);
const char * hostinfo_x86_cpu_name(void); // for example: "x86-64", "x86-64-v4"

#endif 
