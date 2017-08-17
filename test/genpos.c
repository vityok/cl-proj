/*
 * A reference implementation for the general position function
 * test.
 * 
 * Compile with: cc -o genpos genpos.c -lproj
 *
 * Adopted from the geodesic.h/Geodesic API documentation.
 */
#include <geodesic.h>
#include <stdio.h>

int main(int argc, char** argv)
{
  struct geod_geodesic g;
  struct geod_geodesicline l;
  double a12, azi1, lat[101], lon[101];
  int i;
  geod_init(&g, 6378137, 1/298.257223563);
  a12 = geod_geninverse(&g, 40.64, -73.78, 1.36, 103.99,
			0, &azi1, 0, 0, 0, 0, 0);
  printf("a12=%.5f, azi1=%.5f\n", a12, azi1);
  geod_lineinit(&l, &g, 40.64, -73.78, azi1, GEOD_LATITUDE | GEOD_LONGITUDE);
  for (i = 0; i < 101; ++i) {
    geod_genposition(&l, 1, i * a12 * 0.01,
		     lat + i, lon + i, 0, 0, 0, 0, 0, 0);
    printf("%.3f %.3f\n", lat[i], lon[i]);
  }
}
