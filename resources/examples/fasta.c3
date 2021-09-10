module fasta;

const IM = 139968;
const IA = 3877;
const IC = 29573;
const SEED = 42;

uint seed = SEED;

func float fasta_rand(float max)
{
    seed = (seed * IA + IC) % IM;
    return max * seed / IM;
}

private char[] alu =
  "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
  "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
  "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
  "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
  "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
  "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
  "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

extern func int atoi(char *s);
extern func int printf(char *s, ...);
extern func void putchar(int c);

char[] iub = "acgtBDHKMNRSVWY";
double[*] iub_p = {
  0.27,
  0.12,
  0.12,
  0.27,
  0.02,
  0.02,
  0.02,
  0.02,
  0.02,
  0.02,
  0.02,
  0.02,
  0.02,
  0.02,
  0.02 };

char[] homosapiens = "acgt";
double[*] homosapiens_p = {
  0.3029549426680,
  0.1979883004921,
  0.1975473066391,
  0.3015094502008
};

const LINELEN = 60;

// slowest character-at-a-time output
func void repeat_fasta(char[] seq, int n)
{
    usize len = seq.len;
    int i = void;
    for (i = 0; i < n; i++)
    {
        putchar(seq[i % len]);
        if (i % LINELEN == LINELEN - 1) putchar('\n');
    }
    if (i % LINELEN != 0) putchar('\n');
}

func void random_fasta(char[] symb, double[] probability, int n)
{
    assert(symb.len == probability.len);
    int len = probability.len;
    int i = void;
    for (i = 0; i < n; i++)
    {
        double v = fasta_rand(1.0);
        /* slowest idiomatic linear lookup.  Fast if len is short though. */
        int j = void;
        for (j = 0; j < len - 1; j++)
        {
            v -= probability[j];
            if (v < 0) break;
        }
        putchar(symb[j]);
        if (i % LINELEN == LINELEN - 1) putchar('\n');
    }
    if (i % LINELEN != 0) putchar('\n');
}

func void main(int argc, char **argv)
{
    int n=1000;
    if (argc > 1) n = atoi(argv[1]);

    printf(">ONE Homo sapiens alu\n");
    repeat_fasta(alu, n * 2);

    printf(">TWO IUB ambiguity codes\n");
    random_fasta(iub, &iub_p, n * 3);

    printf(">THREE Homo sapiens frequency\n");
    random_fasta(homosapiens, &homosapiens_p, n * 5);

}
