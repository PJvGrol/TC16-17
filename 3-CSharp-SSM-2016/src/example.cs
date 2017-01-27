class Hello
{
    void main()
    {
        int a;
        int b;
        b = 12;
        int c;
        c = 0;
        for(a=1; a < b; a++){
            c += a;
        }
        print(c);
    }
    
    int square( int x)
    {
        return x*x;
    }
    int cube( int x)
    {
        return x*x*x;
    }

}
