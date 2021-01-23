module std::io;

interface File : Closable, Readable, Seekable
{
    FileInfo[]! readdir(int count);
    FileInfo! stat();
}


interface File
{
    inline Closable;
    inline Readable;
    inline Seekable;
    FileInfo[]! readdir(int count);
    FileInfo! stat();
}
