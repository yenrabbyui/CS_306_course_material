void rMinMax(int* a, int i, int j, int& min, int& max)
  {
     if (i == j)
        min = max = a[i];
     else
     {
        int mid = (i + j) / 2;
        int min1, max1;

        rMinMax(a, i, mid, min, max);
        rMinMax(a, mid + 1, j, min1, max1);

        if (min < min1) min = min1;
        if (max > max1) max = max1;
     }
  }
