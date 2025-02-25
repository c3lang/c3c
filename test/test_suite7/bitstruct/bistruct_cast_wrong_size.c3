module bug_repro;

bitstruct Edge_Flags : char {
  bool left;
  bool top;
  bool right;
  bool bottom;
}

fn int main() {
  Edge_Flags edges;
  Edge_Flags a = edges & (Edge_Flags)(1 << 0); // #error: 'int'
  return 0;
}