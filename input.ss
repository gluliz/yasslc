function fatorial( n: integer ): integer
{
 var i, f, a, b: integer;
 i = 2;
 f = 1;
 while( i <= n )
 {
	f = f * i++;
 }
 return f;
}