case class Rational(x : Int, y : Int) :

    require(y > 0 , "y must positive number")

    private def gcd(a : Int, b : Int) : Int =
        if b == 0 then a else gcd(b , a % b)

    private val g = gcd(x,y)
    def numer = x / gcd(x,y)
    def denom = y / gcd(x,y)

    def add(rational : Rational) =
        Rational(x * rational.denom + rational.numer * denom, denom * rational.denom)

    def mul(rational : Rational) = 
        Rational(numer * rational.numer ,denom * rational.denom)

    def neg = Rational(-numer , denom)

    def sub(rational : Rational) = add(rational.neg)

    override def toString() = s"$numer/$denom"        
end Rational


val x = Rational(1,3)
val y = Rational(5,7)
val z = Rational(3,2)
val alpha = x.add(y).mul(z)
alpha.toString()
x.sub(y).sub(z)