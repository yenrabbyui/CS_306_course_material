void sMinMax(int* a, int n, int& min, int& max)
  {
     min = max = a[0];
     for (int i = 1; i < n; i++)
     {
        if (a[i] < min) min = a[i];
        if (a[i] > max) max = a[i];
     }  
  }