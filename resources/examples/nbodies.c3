module nbodies;
import std::io, std::math;

const SOLAR_MASS = 4 * math::PI * math::PI;
const DAYS_PER_YEAR = 365.24;
const DT = 1e-2;
const RECIP_DT = (1.0/DT);

struct Planet
{
	double[<3>] p;
	double[<3>] v;
	double mass;
}

fn void advance(Planet[] bodies) @noinline
{
	sz nbodies = bodies.len;
	foreach (i, Planet* &b : bodies)
	{
		for (sz j = i + 1; j < nbodies; j++)
		{
			Planet* b2 = &bodies[j];
			double[<3>] dp = b.p - b2.p;
			double inv_distance = 1.0 / dp.length();
			double mag = inv_distance * inv_distance * inv_distance;
			b.v -= dp * b2.mass * mag;
			b2.v += dp * b.mass * mag;
		}
	}
	foreach (&b : bodies)
	{
		b.p += b.v;
	}
}

fn double energy(Planet[] bodies)
{
	double e;
	sz nbodies = bodies.len;

	foreach (i, Planet* &b : bodies)
	{
		e += 0.5 * b.mass * b.v.length_sq();
		for (sz j = i + 1; j < nbodies; j++)
		{
			Planet* b2 = &bodies[j];
			double[<3>] dp = b.p - b2.p;
			double distance = dp.length();
			e -= (b.mass * b2.mass) / distance;
		}
	}
	return e;
}

fn void offset_momentum(Planet[] bodies)
{
	double[<3>] momentum;
	foreach (&b : bodies)
	{
		momentum += b.v * b.mass;
	}
	bodies[0].v = - momentum / SOLAR_MASS;
}

Planet[*] planet_bodies = {
	{							   /* sun */
		{0, 0, 0}, {0, 0, 0}, SOLAR_MASS
    },
    {							   /* jupiter */
		{4.84143144246472090e+00,
		-1.16032004402742839e+00,
		-1.03622044471123109e-01},
		{1.66007664274403694e-03 * DAYS_PER_YEAR,
		7.69901118419740425e-03 * DAYS_PER_YEAR,
		-6.90460016972063023e-05 * DAYS_PER_YEAR},
		9.54791938424326609e-04 * SOLAR_MASS
	},
	{							   /* saturn */
		{8.34336671824457987e+00,
		4.12479856412430479e+00,
		-4.03523417114321381e-01},
		{-2.76742510726862411e-03 * DAYS_PER_YEAR,
		4.99852801234917238e-03 * DAYS_PER_YEAR,
		2.30417297573763929e-05 * DAYS_PER_YEAR},
		2.85885980666130812e-04 * SOLAR_MASS
	},
	{							   /* uranus */
		{1.28943695621391310e+01,
		-1.51111514016986312e+01,
		-2.23307578892655734e-01},
		{2.96460137564761618e-03 * DAYS_PER_YEAR,
		2.37847173959480950e-03 * DAYS_PER_YEAR,
		-2.96589568540237556e-05 * DAYS_PER_YEAR},
		4.36624404335156298e-05 * SOLAR_MASS
	},
	{							   /* neptune */
		{1.53796971148509165e+01,
		-2.59193146099879641e+01,
		1.79258772950371181e-01},
		{2.68067772490389322e-03 * DAYS_PER_YEAR,
		1.62824170038242295e-03 * DAYS_PER_YEAR,
		-9.51592254519715870e-05 * DAYS_PER_YEAR},
		5.15138902046611451e-05 * SOLAR_MASS
  }
};


/*
 * Rescale certain properties of bodies. That allows doing
 * consequential advance()'s as if dt were equal to 1.0.
 *
 * When all advances done, rescale bodies back to obtain correct energy.
 */
fn void scale_bodies(Planet[] bodies, double scale)
{
	foreach (&b : bodies)
	{
		b.mass *= scale * scale;
		b.v *= scale;
	}
}


fn void main(String[] args)
{
	int n = args.len < 2 ? 50000000 : args[1].to_int()!!;

	Planet[] bodies = &planet_bodies;
	offset_momentum(bodies);
	double start = energy(bodies);
	scale_bodies(bodies, DT);
	for (int i = 1; i <= n; i++)
	{
		advance(bodies);
	}
	scale_bodies(bodies, RECIP_DT);
	io::printfn("%.9f", start);
	io::printfn("%.9f", energy(bodies));
}
