module nbodies;
import std::io;
import std::math;

const PI = 3.141592653589793;
const SOLAR_MASS = 4 * PI * PI;
const DAYS_PER_YEAR = 365.24;
const DT = 1e-2;
const RECIP_DT = (1.0/DT);

struct Planet
{
	double x, y, z;
	double vx, vy, vz;
	double mass;
}

fn void advance(Planet[] bodies) @noinline
{
	usz nbodies = bodies.len;
	foreach (i, Planet* &b : bodies)
	{
		for (usz j = i + 1; j < nbodies; j++)
		{
			Planet* b2 = &bodies[j];
			double dx = b.x - b2.x;
			double dy = b.y - b2.y;
			double dz = b.z - b2.z;
			double inv_distance = 1.0 / math::sqrt(dx * dx + dy * dy + dz * dz);
			double mag = inv_distance * inv_distance * inv_distance;
			b.vx -= dx * b2.mass * mag;
			b.vy -= dy * b2.mass * mag;
			b.vz -= dz * b2.mass * mag;
			b2.vx += dx * b.mass * mag;
			b2.vy += dy * b.mass * mag;
			b2.vz += dz * b.mass * mag;
		}
	}
	foreach (&b : bodies)
	{
		b.x += b.vx;
		b.y += b.vy;
		b.z += b.vz;
	}
}

fn double energy(Planet[] bodies)
{
	double e;
	usz nbodies = bodies.len;

	foreach (i, Planet* &b : bodies)
	{
		e += 0.5 * b.mass * (b.vx * b.vx + b.vy * b.vy + b.vz * b.vz);
		for (usz j = i + 1; j < nbodies; j++)
		{
			Planet* b2 = &bodies[j];
			double dx = b.x - b2.x;
			double dy = b.y - b2.y;
			double dz = b.z - b2.z;
			double distance = math::sqrt(dx * dx + dy * dy + dz * dz);
			e -= (b.mass * b2.mass) / distance;
		}
	}
	return e;
}

fn void offset_momentum(Planet[] bodies)
{
	double px;
	double py;
	double pz;
	foreach (&b : bodies)
	{
		px += b.vx * b.mass;
		py += b.vy * b.mass;
		pz += b.vz * b.mass;
	}
	bodies[0].vx = - px / SOLAR_MASS;
	bodies[0].vy = - py / SOLAR_MASS;
	bodies[0].vz = - pz / SOLAR_MASS;
}

Planet[*] planet_bodies = {
	{							   /* sun */
		0, 0, 0, 0, 0, 0, SOLAR_MASS
    },
    {							   /* jupiter */
		4.84143144246472090e+00,
		-1.16032004402742839e+00,
		-1.03622044471123109e-01,
		1.66007664274403694e-03 * DAYS_PER_YEAR,
		7.69901118419740425e-03 * DAYS_PER_YEAR,
		-6.90460016972063023e-05 * DAYS_PER_YEAR,
		9.54791938424326609e-04 * SOLAR_MASS
	},
	{							   /* saturn */
		8.34336671824457987e+00,
		4.12479856412430479e+00,
		-4.03523417114321381e-01,
		-2.76742510726862411e-03 * DAYS_PER_YEAR,
		4.99852801234917238e-03 * DAYS_PER_YEAR,
		2.30417297573763929e-05 * DAYS_PER_YEAR,
		2.85885980666130812e-04 * SOLAR_MASS
	},
	{							   /* uranus */
		1.28943695621391310e+01,
		-1.51111514016986312e+01,
		-2.23307578892655734e-01,
		2.96460137564761618e-03 * DAYS_PER_YEAR,
		2.37847173959480950e-03 * DAYS_PER_YEAR,
		-2.96589568540237556e-05 * DAYS_PER_YEAR,
		4.36624404335156298e-05 * SOLAR_MASS
	},
	{							   /* neptune */
		1.53796971148509165e+01,
		-2.59193146099879641e+01,
		1.79258772950371181e-01,
		2.68067772490389322e-03 * DAYS_PER_YEAR,
		1.62824170038242295e-03 * DAYS_PER_YEAR,
		-9.51592254519715870e-05 * DAYS_PER_YEAR,
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
		b.vx *= scale;
		b.vy *= scale;
		b.vz *= scale;
	}
}


fn void main(char[][] args)
{
	int n = args.len < 2 ? 50000000 : str::to_int(args[1])!!;

	Planet[] bodies = &planet_bodies;
	offset_momentum(bodies);
	double start = energy(bodies);
	scale_bodies(bodies, DT);
	for (int i = 1; i <= n; i++)
	{
		advance(bodies);
	}
	scale_bodies(bodies, RECIP_DT);
	io::printfln("%.9f", start);
	io::printfln("%.9f", energy(bodies));
}
