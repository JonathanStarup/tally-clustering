namespace Tally {

    enum Tally with Eq {
        case Nothing
        case Line(Tally)
        case DashCluster(Tally)
    }


    pub def zero(): Tally =
        Nothing
    pub def one(): Tally =
        Line(Nothing)
    pub def three(): Tally =
        Line(Line(Line(Nothing)))
    pub def seven(): Tally =
        Line(Line(DashCluster(Nothing)))


    /// adds another to the running count.
    /// O(4) complexity
    pub def tally(t: Tally): Tally = match t {
        case Line(Line(Line(Line(remaining)))) => DashCluster(remaining)
        case other => Line(other)
    }

    def tallyN(n: Int32, t: Tally): Tally = {
        if (n <= 0) t else tallyN(n - 1, tally(t))
    }

    @Test
    def zeroBecomesOne(): Bool = tally(zero()) == one()

    @Test
    def oneBecomesThree(): Bool = tallyN(2, one()) == three()

    @Test
    def threeBecomesSeven(): Bool = tallyN(4, three()) == seven()

    /// count the number that `t` represents
    /// O(n/5) complexity
    pub def count(t: Tally): Int32 = countTail(t, 0)

    /// count the number that `t` represents
    /// O(n/5) complexity
    def countTail(t: Tally, acc: Int32): Int32 = match t {
        case Nothing => acc
        case Line(remaining) => countTail(remaining, acc + 1)
        case DashCluster(remaining) => countTail(remaining, acc + 5)
    }


    @Test
    def countZero(): Bool = count(zero()) == 0

    @Test
    def countOne(): Bool = count(one()) == 1

    @Test
    def countThree(): Bool = count(three()) == 3

    @Test
    def countSeven(): Bool = count(seven()) == 7

    @Test
    def count1024(): Bool = count(tallyN(1024, zero())) == 1024

}
