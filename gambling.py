import itertools as itr

cards = ["A"] + [str(i) for i in range(1, 11)] + ["K","Q","B","J"]
r = 1 / len(cards)

def briber(h, b):
    def f(you, dealer, bribe):
        if you < 12:
            return (True, 0)
        else:
            if you < 21:
                if you >= b:
                    bribe = bribe if bribe else 2

                if you < h:
                    return (True, bribe)
                else:
                    return (False, bribe)
            else:
                return (False, bribe)
    return f

def basic(h):
    def f(you, dealer, _):
        return (you < dealer or you < h, 0)
    return f

# simplified don't care
def value(hand, card):
    if card == "A":
        if hand > 10:
            return 1
        else:
            return 11

    if card in ["K","Q","B","J"]:
        return 10

    return int(card)

def pgame():
    n = 0
    p = 0
    for (a, b) in itr.combinations_with_replacement(cards, 2):
        for (c, d) in itr.combinations_with_replacement(cards, 2):
            n += 1
            you = value(0, a)
            you += value(you, b)
            dealer = value(0, c)
            dealer += value(you, d)

            p += hitw(you, dealer, 0)

    return p / n

def Egame():
    n = 0
    E = 0
    for (a, b) in itr.combinations_with_replacement(cards, 2):
        for (c, d) in itr.combinations_with_replacement(cards, 2):
            n += 1
            you = value(0, a)
            you += value(you, b)
            dealer = value(0, c)
            dealer += value(you, d)

            E += Ehit(you, dealer, 0)

    return E / n

standw_t = {}
def standw(you, dealer):
    if (you, dealer) in standw_t:
        return standw_t[(you, dealer)]
    
    if dealer > 21:
        p = 1
    elif you <= dealer:
        p = 0
    elif dealer >= 17:
        p = 1
    else:
        p = r * sum(standw(you, dealer + value(dealer, card)) for card in cards)

    standw_t[(you, dealer)] = p
    
    return p

hitw_t = {}
def hitw(you, dealer, bribe):
    if (you, dealer, bribe) in hitw_t:
        return hitw_t[(you, dealer, bribe)]

    bribe = 0
    if dealer == 21:
        return 0
    elif you == 21:
        return 1
    elif you > 21:
        p = 0
    else:
        (h, bribe_) = strat(you, dealer, bribe)
        bribe_ /= 4

        if h:
            pnb = r * sum(hitw(you + value(you, card), dealer, max(0, bribe_ - 0.25)) for card in cards)

            if you > 10:
                pb = 1
            else:
                pb = hitw(you + 10, dealer, max(0, bribe_ - 0.25))

            p = (1 - bribe_) * pnb + bribe_ * pb
        else:
            p = standw(you, dealer)

    hitw_t[(you, dealer, bribe)] = p
    return p

# E is return, profit is E - 1
def Estand(you, dealer):
    p = standw(you, dealer)
    return p

Ehit_t = {}
def Ehit(you, dealer, bribe):
    if (you, dealer, bribe) in Ehit_t:
        return Ehit_t[(you, dealer, bribe)]

    bribe = 0
    if dealer == 21:
        E = 0
    elif you == 21:
        E = 1
    elif you > 21:
        E = 0
    else:
        (h, bribe_) = strat[0](you, dealer, bribe)
        bribe__ = bribe_ / 4

        if h:
            Enb = r * sum(Ehit(you + value(you, card), dealer, max(0, bribe__ - 0.25)) for card in cards)

            if you > 10:
                Eb = 1
            else:
                Eb = Ehit(you + 10, dealer, max(0, bribe__ - 0.25))

            E = (1 - bribe__) * Enb + bribe__ * Eb

            if bribe_ > bribe:
                E *= 0.75 ** bribe_
        else:
            E = Estand(you, dealer)

    Ehit_t[(you, dealer, bribe)] = E
    return E

# TODO handle push

def Egame_net():
    return Egame() * mult - 1

mult = 2.5
strat = [None]
test = True
Emax, imax, jmax = -100, 0, 0

if test:
    for i in range(8, 20):
        for j in range(8, 20):
            strat[0] = briber(i, j)
            hitw_t = {}
            standw_t = {}
            Ehit_t = {}
            E_ = Egame()
            if E_ > Emax:
                Emax = E_
                imax = i
                jmax = j
            print(i, j, E_)

print(imax, jmax, Emax)
#i = 18
#j = 8
#E = 0.4608717907658956
print(Emax * mult - 1)
