-module(gcd).
-export([gcd/2]).
 
 
gcd(M, 0) -> M;
gcd(M, N) -> gcd(N, M rem N).
