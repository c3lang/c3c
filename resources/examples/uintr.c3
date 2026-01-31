module uintr;
/* This example requires:
 * uintr enabled kernel
 * uintr enabled Intel CPU
 */
import std::os::posix;
import libc::os;
import libc;


macro senduipi(v) {
  CULongLong internal = v;
  asm {
   senduipi internal;
  }
}

macro stui() {
  asm {
   stui;
  }
}

macro clui(){
  asm {
   clui;
  }
}


const int NR_UINTR_REGISTER_HANDLER = 471;
const int NR_UINTR_UNREGISTER_HANDLER = 472;
const int NR_UINTR_CREATE_FD = 473;
const int NR_UINTR_REGISTER_SENDER = 474;
const int NR_UINTR_UNREGISTER_SENDER = 475;
const int NR_UINTR_WAIT = 476;        

macro uintr_register_handler(handler, flags) {
  return $$syscall(NR_UINTR_REGISTER_HANDLER, handler, flags);
}
macro uintr_unregister_handler(flags){
  return $$syscall(NR_UINTR_UNREGISTER_HANDLER, flags);
}
macro uintr_create_fd(vector, flags){
  return $$syscall(NR_UINTR_CREATE_FD, vector, flags);
}
macro uintr_register_sender(fd, flags){
  return $$syscall(NR_UINTR_REGISTER_SENDER, fd, flags);
}
macro uintr_unregister_sender(ipi_idx, flags){
  return $$syscall(NR_UINTR_UNREGISTER_SENDER, ipi_idx, flags);
}
macro uintr_wait(flags){
  return $$syscall(NR_UINTR_WAIT, flags);
}


struct Uintr_frame
{
  /* RIP of the interrupted user process.  */
  CULongLong rip;
  /* RFLAGS of the interrupted user process.  */
  CULongLong rflags;
  /* RSP of the interrupted user process.  */
  CULongLong rsp;
}


/* for now implement the handler with C */
extern fn void ui_handler(Uintr_frame* ui_frame, CULongLong vector);

fn void sender(void* arg) {
  int uintr_fd = (int)arg;
  CULongLong uipi_index = (CULongLong) uintr_register_sender(uintr_fd, 0);

  while(true){
    senduipi(uipi_index);
    
  }
}

fn void receiver() {
  while(true){}
}

fn int main(){
  int err;
  std::thread::os::Pthread_attr_t attrs;
  std::os::posix::Pthread_t th;
  int handler = (int) uintr_register_handler(&ui_handler, 0);
  if (handler) {
    libc::perror("ERR Register handler");
    return -1;
  }
  
  int uintr_fd = (int) uintr_create_fd(0,0); 
  err = std::os::posix::pthread_attr_init(&attrs);
  if(err){
    libc::errno_set((Errno)err);
    libc::perror("ERR ATTR");
    return -1;
  }
 
  err = std::os::posix::pthread_create(&th,&attrs, &sender, (void*)uintr_fd);  

  if(err){
    libc::errno_set((Errno)err);
    libc::perror("ERR CREAT");
    return -1;
  }

  receiver();
  
}
