#include <ctime>
#include <iostream>
#include <iomanip>
#include <string>
#include <sys/time.h>
using namespace std;

/*
 * The famous fibonacci function, recursive version.
 */
long fibonacci(long n)
{
   if (n == 0 || n == 1)
   {
      return n;
   }
   else
   {
      return (fibonacci(n - 1) + fibonacci(n - 2));
   }
}

void testFibonacci()
{
   clock_t start = clock();
   long fib42 = fibonacci(42);
   clock_t finish = clock();
   cout << "\nTo compute fibonacci(42) = " << fib42
        << "\nrecursively took\n\n"
        << setprecision(5) << setw(4) << (finish - start) / (double) CLOCKS_PER_SEC
        << " seconds.\n\n";
   cout << "-------------------------\n" << endl;
}

long fibmem[100] = {0};

/*
 * A "memory-function" implementation of the fibonacci function.
 */
long memFuncFibonacci(long n)
{
   if (fibmem[n] == 0)
   {
      if (n == 0 || n == 1)
      {
         fibmem[n] = n;
      }
      else
      {
         fibmem[n] = (memFuncFibonacci(n - 1) + memFuncFibonacci(n - 2));
      }
   }
   return fibmem[n];
}

void testMemFuncFibonacci()
{
   long double usec1;
   long double usec2;
   long double elapsed;
   timeval start;
   timeval finish;
   long fib42;

   gettimeofday(&start, NULL);
   fib42 = memFuncFibonacci(42);
   gettimeofday(&finish, NULL);
   usec1 = (long double) (start.tv_sec) +
      (long double) (start.tv_usec / 1000000.0f);
   usec2 = (long double) (finish.tv_sec) +
      (long double) (finish.tv_usec / 1000000.0f);
   elapsed = usec2 - usec1;

   cout.setf(ios::fixed);
   cout << "To compute fibonacci(42) = " << fib42
        << "\nwith a memory function took\n\n"
        << setprecision(11) << setw(13) << elapsed
        << " seconds.\n\n";
   cout << "-------------------------\n" << endl;
}

/*
 * Run tests.
 */
int main()
{
   testFibonacci();
   testMemFuncFibonacci();
}

