class Hello
{
    int g;
    
    void main()
    {
        int b;
        b=5*3==8 || 4*6>7 && 5*3==15;
    }
    
    int square( int x )
    {
        int y;
        y = x*x;
        return y;   
    }

    int abs(int x)
    {
    	
        if (x<0)
            x = 0-x;
        return x;
    }
    
    int fac(int x)
    {
        int r; int t;
        t=1; r=1;
        while (t<=x)
        {
            r = r*t;
            t = t+1;
        }
        return r;
   }
}
