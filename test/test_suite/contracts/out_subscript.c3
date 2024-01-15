/**
 * @param [out] z
 * @param [out] out_data
 **/
fn void tes2t(char* z, char[] out_data, char[] in_data) {
	z[0] = 2;
	z[0] += 1; // #error: 'out' parameters may not be read
	out_data[0] = 3;
	out_data[0] += 1;  // #error: 'out' parameters may not be read
	out_data[0..3]; // #error: 'out' parameters may not be read
	out_data[0..3] = 23;
}