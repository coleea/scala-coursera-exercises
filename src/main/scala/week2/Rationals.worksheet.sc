case class Rational(x : Int, y : Int) :

    require(y > 0 , "y must positive number")

    def this(x : Int) = this(x , 1)

    private def gcd(a : Int, b : Int) : Int =
        if b == 0 then a else gcd(b , a % b)

    def numer = x
    def denom = y

    def less(r : Rational) : Boolean = numer * r.denom < r.numer * denom

    def more(rational : Rational) = 
        numer * rational.denom > rational.numer * denom

    def add(rational : Rational) =
        Rational(x * rational.denom + rational.numer * denom, denom * rational.denom)

    def mul(rational : Rational) = 
        Rational(numer * rational.numer ,denom * rational.denom)

    def neg = Rational(-numer , denom)

    def sub(rational : Rational) = add(rational.neg)

    override def toString() = 
        val gcded = gcd(numer, denom.abs)
        val numberGcded = numer / gcded
        val denomGcded = denom / gcded
        s"$numberGcded/$denomGcded"                
    end toString

end Rational

extension (r : Rational) 
    infix def min(s : Rational) : Boolean = if s.less(r) then true else false
    def abs : Rational = Rational(r.numer.abs, r.denom)
    def +(s : Rational) : Rational = r.add(s)
    def -(s : Rational) : Rational = r.sub(s)
    def *(s : Rational) : Rational = r.mul(s)    
    def <(s : Rational) : Boolean = r.less(s)
    def >(s: Rational) : Boolean = r.more(s)

end extension

val x = Rational(1,3)
val y = Rational(5,7)
val z = Rational(3,2)
val a = Rational(3,2)
val alpha = x.add(y).mul(z)
alpha.toString()
x.sub(y).sub(z)
x - y - z
x min y
y min x
x.min(y)
x < y
x > y
Rational(1,2).numer
// [1/x, 2/y] [] [Rational(1,2) / this] x
// (((x + y) ^? (z ?^ a)) less ((x ==> y) | z))