function main(arg:integer):integer
{
	var x:integer;
	var y:integer;
	var z:integer;
	x = 3;
	y = 4;
    z = 5;
    z = x + y;
    z = 2 * (z - 3) / 5;
}

E -> E ‘+’ T
| T
T -> T ‘*’ F
| F
F -> ’(‘ E ‘)’
| ID

a*(b+c)$
