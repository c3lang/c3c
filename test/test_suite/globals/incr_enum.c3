// #skip

type State enum u8 {
    A,
    B,
}

State += 10;        // @error{expected identifier after incremental enum}


type State enum u8 {
    A,
    B,
}

State += C;
State += D;
