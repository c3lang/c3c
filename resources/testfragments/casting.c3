struct Pixmap
{
    char *p;
    int x;
    int y;
    int bpp;
}

fn void readpgm(char* name, Pixmap* p)
{
    /* ... */
    pnm_readpaminit(fp, &inpam);
    p.x = inpam.width;
    p.y = inpam.height;
    if (!(p.p = malloc(@safe(p.x + p.y)))
    {
        @F1("Error at malloc");
    }
    for (int i = 0; i < inpam.height; i++)
    {
        pnm_readpamrow(&inpam, tuplerow)
        {
            for (int j = 0; j < inpam.width; j++)
            {
                p.p[@nowrap(i * inpam.width + j)] = sample;
            }
        }
    }
}

fn void getComm(uint len, char* src)
{
    uint size;
    size = len - 2;
    char* comm = malloc(size + 1);
    memcpy(comm, src, size);
}

fn uint* decode_fh(uint* p, SvcFh* fhp)
{
    int size;
    fh_init(fhp, NFS3_FHSIZE);
    size = ntohl(*p++);
    if (size > NFS3_FHSIZE) return NULL;
    memcpy(&fhp.fh_handle.fh_base, p, size);
    fhp.fh_handle.fh_size = size;
    return p + XDR_QUADLEN(size);
}
