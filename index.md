---
title       : Option Pricing Tool
subtitle    : Effects of Volatility on Option Prices
author      : John Elliott
job         : Data Products
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Option Pricing Tool

The Option Pricing Tool calculates the option prices for two types of option contracts. 
The contracts are leveraged financial vehicles controlling 100 shares of the underlying stock. Both types of contracts have an "Expiration" date and are deemed void and worthless after that date.

* Calls - A contract where the owner has the "Option" to buy the shares at an agreed upon price called the "strike price" of the contract.

* Puts - A contract where the owner has the "Option" to sell the shares at an agreed upon price called the "strike price" of the contract. 

### Option Pricing Tool Details
1. The Option Pricing Tool gives the option to enter user data or use pre-loaded data
2. Code available on GITHub at [GIThub](http://github.com/DrJohnElliott/Option_Pricer.git)
3. The Option Pricing Tool is hosted on [Shinnyapps](http://drjohn.shinyapps.io/Option_Pricer/)

--- 

## Black-Scholes Model used for Option Pricing

![width](blackscholes.png)


The R function pnorm is used to give the cumulative normal distribution function in the model 

source:(http://www.investopedia.com/university/options-pricing/black-scholes-model.asp)

---

## Example 



The effects of volatility on option prices can lead to undesirable results when the effects are not realized. The Option Pricing Tool allows the user to examine the effects of changing volatility on option prices.
Lets calculate the price of some options. If the stock TESLA has a market price of 186.80 USD, strike price of 180 USD, and Volatility is estimated at 35% and an expiration in 30 days. 
Here we will use the function option_price inline in the document to calculate CALL price ie. "r option_price(186.8,180,35,1,1,30)[1] " and the using the suffix [2] for the PUT price

* Call price: 11.22
* Put price: 4.43

If the Stock goes up 4 USD to 190.8 but volatility drops to 10% and keeping all other variables constant, your contracts would decrease in value.

* Call price: 10.83
* Put price: 0.04

This example shows why it is important to understand the effects of volatility on option prices.

---

## Function used to calculate Option Prices

```r
option_price <- function(u_p,x_p,v,i_r,d,t_e){
        ln_ratio        <- log(u_p/x_p)
        numerator_1     <- (i_r - d + v^2/2)*t_e
        denominator_1   <- v * sqrt(t_e)
        formula_1       <- (ln_ratio+numerator_1)/denominator_1
        formula_2       <- formula_1- denominator_1
        norm_d1         <- pnorm(formula_1,0,1)
        norm_d2         <- pnorm(formula_2,0,1)
        norm_neg_d1     <- pnorm(-formula_1,0,1)
        norm_neg_d2     <- pnorm(-formula_2,0,1)
        exponents_1_2   <- exp(-i_r*t_e)
        X_e_rt          <- x_p * exponents_1_2
        e_qt            <- exp(-d*t_e)
        S0_e_qt         <- u_p*e_qt
        Call_option_price       <- round(S0_e_qt * norm_d1 - X_e_rt * norm_d2,digits = 2)
        Put_option_price        <- round(X_e_rt*norm_neg_d2-S0_e_qt*norm_neg_d1, digits = 2)
        option_price            <- c(Call_option_price,Put_option_price)
        return(option_price )
}
```
