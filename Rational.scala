/**
 *A Rational number is a number that can be
 *expressed as a ratio n/d, where n and d are
 *integers, except that d cannot be zero. n is called 
 *the numerator and d the denominator
 */
class Rational (n: Int, d: Int) {
	private def divisor = gcd(n.abs, d.abs)
	val numerator = n 
	val denominator = d

	private def gcd(a:Int, b:Int): Int = {
		if(b == 0)
			a
		else gcd(b,a%b)
	}

	def this(n: Int) = this(n,1) // n/1 
	def add(b: Rational): Rational ={
		new Rational(
			numerator *b.denominator + denominator*b.numerator,
			denominator * b.denominator
		)
	}
	def subtract(b: Rational): Rational ={
		new Rational( numerator * b.denominator - denominator * b.numerator,
			denominator * b.denominator		
		)
	}
	def multiply(b:Rational): Rational = {
		new Rational (numerator * b.numerator,denominator * 
			b.denominator
		)
	}


	def divide(b:Rational): Rational = {
		new Rational(
			numerator * b.denominator, denominator * b.numerator
			)
	}


}