module fasta;
import std::io;
import libc;

const IM = 139968;
const IA = 3877;
const IC = 29573;
const SEED = 42;

uint seed = SEED;

fn float fasta_rand(float max_val)
{
	seed = (seed * IA + IC) % IM;
	return max_val * seed / IM;
}

String alu @private =
	"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
	"GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
	"CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
	"ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
	"GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
	"AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
	"AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";


String iub = "acgtBDHKMNRSVWY";
double[] iub_p = {
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

String homosapiens = "acgt";
double[] homosapiens_p = {
	0.3029549426680,
	0.1979883004921,
	0.1975473066391,
	0.3015094502008
};

const LINELEN = 60;

// slowest character-at-a-time output
fn void repeat_fasta(String seq, int n)
{
	usz len = seq.len;
	int i @noinit;
	for (i = 0; i < n; i++)
	{
		io::putchar(seq[i % len]);
		if (i % LINELEN == LINELEN - 1) io::putchar('\n');
	}
	if (i % LINELEN != 0) io::putchar('\n');
}

fn void random_fasta(String symb, double[] probability, int n)
{
	assert(symb.len == probability.len);
	int len = probability.len;
	int i @noinit;
	for (i = 0; i < n; i++)
	{
		double v = fasta_rand(1.0);
		/* slowest idiomatic linear lookup.  Fast if len is short though. */
		int j @noinit;
		for (j = 0; j < len - 1; j++)
		{
			v -= probability[j];
			if (v < 0) break;
		}
		io::putchar(symb[j]);
		if (i % LINELEN == LINELEN - 1) io::putchar('\n');
	}
	if (i % LINELEN != 0) io::putchar('\n');
}

fn int main(int argc, char **argv)
{
	int n = 1000;
	if (argc > 1) n = libc::atoi(argv[1]);

	io::printf(">ONE Homo sapiens alu\n");
	repeat_fasta(alu, n * 2);

	io::printf(">TWO IUB ambiguity codes\n");
	random_fasta(iub, iub_p, n * 3);

	io::printf(">THREE Homo sapiens frequency\n");
	random_fasta(homosapiens, homosapiens_p, n * 5);

	return 0;
}
