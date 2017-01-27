class Hello
{
    int g;
    int a;
    
    void main()
    {
        a = 3;
        g = 0;
        g = 8 - 3 + (a++);
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
