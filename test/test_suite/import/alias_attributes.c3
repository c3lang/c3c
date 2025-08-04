module abc;

alias test @if(true) = module abc; // #error: Module aliases cannot