# Exchange Rate Policy
SMU ECON248-Financial Market and Monetary Policy Project <br><br>
Grade: A+

<h1>Summary</h1>
The central bank, State Bank of Vietnam (SBV), plays a crucial role in managing the country’s monetary policy to achieve its macroeconomic goals such as economic growth and price stability. 
Vietnam’s development will pose several challenges ahead for the country, underlining the importance of a strategic and effective monetary policy that will help Vietnam transition from a developing economy.<br><br>

As the country progresses and opens to increasing trade and capital flows, we posit that exchange rate targeting will be the more effective instrument for Vietnam in achieving economic growth than money supply. 
The impact of Covid-19 reinforces the need for a focus on economic growth over price stability. Hence, we will examine the possibility of using exchange rate as a monetary policy instrument for the intermediate to long term horizon.

In our report, we go over the economic environment of Vietnam, analyze the existing measures of the SBV and case studies from other countries using exchange rate as a monetary policy. 
However, as far as data analysis is concerned, we attempt to support our argument that an exchange rate policy is an effective channel for monetary policy using a Vector Autoregressive (VAR) model in R.

<h1>VAR Model</h1>
A Vector Autoregressive (VAR) model is a multivariate time series model in which there are 2 or more time series that influence each other. It is autoregressive as each variable can be represented as a function of past values that is:

![Equation_ARp_Model-min](https://user-images.githubusercontent.com/88301287/169814249-eb5a2a95-d4e1-4c3a-aa08-0785326155f5.png)

At the same time, the difference between a VAR and normal Autoregressive (AR) time series is the pressence of more than 1 time series that influence each other, that is, the relationship between them is bi-directional in VAR while only uni-directional in ARs.

![Equation_VAR1_Model-min](https://user-images.githubusercontent.com/88301287/169814275-15ca3bed-6df5-4f27-8dd6-23f73593a43d.png)

In macroeconomics, we notice that it is indeed the case that the relationship between macroeconomic variables is bi-directional. As a simple example, an increase in GDP from AD may cause inflation when a country is near the AS or full capacity.
However, rising prices may also cause consumers to hold back on spending resulting in decrease in GDP. Therefore, evidently macroeconomic variables are time series in nature as well.

<h1>Data and Methodology</h1>
Data used in the VAR model was taken from CEIC database and the table below shows the types of data collected. Quarterly data was collected to determine if the analysis was consistent throughout for the period of 2002 to 2019, and analysis was conducted in R using the <code>vars</code> package.
<br><br>

|                            Data   Type                          |                             Usage                           |
|:---------------------------------------------------------------:|:-----------------------------------------------------------:|
|     Consumer   Price Index (CPI) of all Items in Vietnam        |     Interpretation   of Change in Price Level in Vietnam    |
|     Nominal   GDP in Vietnam                                    |     Interpretation   of GDP Growth in Vietnam               |
|     Volume   of Broad Money (M2) in Vietnam                     |     Effects   of Monetary Targeting                         |
|     Official   Rates at End of a Period in Vietnam (VND/USD)    |     Effects   of Exchange Rate Targeting                    |

Data collected was then converted into the logarithmic form to reflect relative percentage changes in the variables. For example, an increase in the Log (GDP) would represent a relative percentage increase in GDP which we can interpret as positive economic growth.

In order to evaluate the effectiveness of both the exchange rate channel and the monetary channel, we will then check the Impulse Response Functions and Variance Decomposition graphs.

<h1>Exploratory Data Analysis</h1>
Begin we begin, we can first examine the statistical properties of the data.

    vietnam_quarterly<-read.csv("./vietnam quarterly data.csv")
    
    #converts columns into time series objects
    gdp<-ts(log(vietnam_quarterly$GDP),start=c(2002,2),frequency = 4)
    cpi<-ts(vietnam_quarterly$CPI,start=c(2002,2),frequency = 4)
    exr<-ts(log(vietnam_quarterly$EXR),start=c(2002,2),frequency = 4)
    m2<-ts(log(vietnam_quarterly$M2),start=c(2002,2),frequency = 4)
    ir<-ts(vietnam_quarterly$IR,start=c(2002,2),frequency = 4)
    liquid<-ts(vietnam_quarterly$LIQUID,start=c(2012,2),frequency = 4)
    
    plot(cbind(gdp,cpi,exr,m2),main = "time series plots")
    
This results in the following plot:<br>
![image](https://user-images.githubusercontent.com/88301287/169817366-39372387-eab5-42f0-b7f9-f03ab423eadd.png)
 
The time series plots of the nominal GDP showed clear evidence of seasonality which will need to be taken care of.
 
Other statistics can be obtained from the codes:

    adf.test(gdp) #all are in fact non-stationary which is not ideal.
    adf.test(cpi)
    adf.test(exr)
    adf.test(m2)

Moving on, we can build the VAR model using the code:

    newdf<-na.omit(cbind(gdp,cpi,exr,m2))
    colnames(newdf)<-c("gdp","cpi","exr","m2")

    #select the best number of lags to include in the model
    bestlag<-VARselect(newdf,type = "both")
    bestlag$selection
    
    #based on the AIC criteria, the best lag is 4.
    #as there is quarterly seasonality, the season parameter is 4.
    var1<-VAR(newdf,p=4,type="both",season = 4, exogen =NULL)

    plot(stability(var1, type="OLS-CUSUM"),nc=2)
    
The stability plot of the model shows that it is stable.


<h1>Impulse Response Functions</h1>
The IRF predicts the sign, magnitude, and statistical significance of the response of a variable to an increase or shock to another variable. To create the IRFs of the VAR model, we input the following codes:

    newdf<-na.omit(cbind(gdp,cpi,exr,m2))
    colnames(newdf)<-c("gdp","cpi","exr","m2")

    #select the best number of lags to include in the model
    bestlag<-VARselect(newdf,type = "both")
    bestlag$selection
    
    #based on the AIC criteria, the best lag is 4.
    #as there is quarterly seasonality, the season parameter is 4.
    var1<-VAR(newdf,p=4,type="both",season = 4, exogen =NULL)

    #impulse response of gdp to exchange rate and to money supply
    gdpirf1<-irf(var1,impulse = "exr",response = "gdp",boot = T,n.ahead = 40)
    gdpirf2<-irf(var1,impulse = "m2",response = "gdp",boot = T,n.ahead = 40)

    #impulse response of cpi to exchange rate and to money supply
    cpiirf1<-irf(var1,impulse = "exr",response = "cpi",boot = T,n.ahead = 40)
    cpiirf2<-irf(var1,impulse = "m2",response = "cpi",boot = T,n.ahead = 40)

    plot(gdpirf1,ylab="GDP",main="Shock to Exchange Rate")
    plot(gdpirf2,ylab="GDP",main="Shock to M2")
    plot(cpiirf1,ylab="CPI",main="Shock to Exchange Rate")
    plot(cpiirf2,ylab="CPI",main="Shock to M2")

These result in the following graphs:
![image](https://user-images.githubusercontent.com/88301287/169819434-5fc6af06-dc7e-4ea2-b40a-2b69a7487a28.png)

According to the IRFs, an increase in the exchange rates, or appreciation of the VND, will initially decrease GDP but that will quickly be turned into an increase in GDP at about the 8th quarter. After which, the response becomes negative again in the longer term after about ten quarters and the response is statistically significant. 
Meanwhile, an increase in M2 will have a sharp increase in the GDP in the shorter to medium term but will slowly decrease after 12 quarters and the response is not statistically significant.<br><br>
The results are consistent with the theory that when exchange rate appreciates, foreign goods denominated in USD would be relatively cheaper thus there would be greater imports and lower exports. Furthermore, increase in money supply would increase the GDP since there will be a rise in consumption and investment.<br><br>
When it comes to achieving SBV’s goal of economic growth, we predict that targeting M2 would be a better policy option in the shorter to medium term since the response (approximately 0.009%) is statistically significant and larger than that of exchange rate (approximately 0.005%) in the shorter term. Meanwhile, targeting exchange rates in the long term would make for the better policy option since the magnitude of the response (-0.01%) is statistically significant and larger than that of M2 (0.004%) in the longer term.

![image](https://user-images.githubusercontent.com/88301287/169819852-b5cdb33a-17b7-4923-bd46-30d1b081ded9.png)

As for the IRF of CPI, the results suggest that an increase in the exchange rates, or appreciation of VND, will cause an increase in the CPI, thereby causing inflation, in the short to medium term but the response becomes negative after approximately 16 quarters. Meanwhile for a shock to M2, it will similarly cause an increase in the CPI but peak out faster in about eight quarters although the response will reverse again shortly after, and this suggests greater inflation rates in the future. 
The response to exchange rates is much greater in magnitude than the response to M2.<br><br>
The results are consistent with the theory that when exchange rate appreciates, the shock in exchange rate would result in imported inflation in the shorter term but in general, it should alleviate inflationary pressures since local demand would decrease. Furthermore, from a monetarist point of view, increase in the money supply faster than the increase in GDP would cause inflation since MV=PY, and there would be more money in the market chasing fewer goods. 
This would result in an increase in prices, and we observe in that the response of CPI is much greater than that of GDP.<br><br>
When it comes to achieving the SBV’s goal of price stability, however, both targeting M2 and exchange rate via a depreciation would result in inflationary pressures indicated by a positive impact on CPI. Targeting M2 could be a better policy option since the response is statistically significant and smaller than that of exchange rate in all time periods. 
However, there are also more fluctuations in the response of CPI to M2, which would make it more difficult to formulate a consistent monetary policy for price stability.

<h1>Variance Decomposition</h1>
The variance decompositions of each factor in the VAR model are generated to analyse the impact that M2 and exchange rate have on GDP and CPI in Vietnam in the future. Time intervals of three months, six months, one year, five years, and ten years have been set to observe the short and long run effects.
Since we are looking a custom time frame that includes 40 periods (10 years * 4 quarters), the default plots do not look aesthetically pleasing and hence we have chosen to use <code>ggplot</code> to remake the graphs.

    fevd1<-fevd(var1,n.ahead = 120)
    timeframe<-factor(c("1 month","6 months", "1 year","5 years","10 years"))
    variables<-factor(c("m2","exr","cpi","gdp"))

    res1<-fevd1$gdp[c(1,6,12,60,120),]
    res2<-fevd1$cpi[c(1,6,12,60,120),]
    res3<-fevd1$exr[c(1,6,12,60,120),]
    res4<-fevd1$m2[c(1,6,12,60,120),]

    res1<-cbind(data.frame(res1),timeframe)
    res2<-cbind(data.frame(res2),timeframe)
    res3<-cbind(data.frame(res3),timeframe)
    res4<-cbind(data.frame(res4),timeframe)

    res1<-res1%>%gather(variable,percent,-timeframe)
    res2<-res2%>%gather(variable,percent,-timeframe)
    res3<-res3%>%gather(variable,percent,-timeframe)
    res4<-res4%>%gather(variable,percent,-timeframe)

    res1$timeframe<-factor(res1$timeframe,levels = timeframe)
    res2$timeframe<-factor(res2$timeframe,levels = timeframe)
    res3$timeframe<-factor(res3$timeframe,levels = timeframe)
    res4$timeframe<-factor(res4$timeframe,levels = timeframe)

    res1$variable<-factor(res1$variable,levels = variables)
    res2$variable<-factor(res2$variable,levels = variables)
    res3$variable<-factor(res3$variable,levels = variables)
    res4$variable<-factor(res4$variable,levels = variables)

    g1<-ggplot(res1, aes(fill=variable, y=percent, x=timeframe)) + 
      geom_bar(position="stack", stat="identity") +
      labs(title="Variance Decomposition of GDP") + 
      scale_fill_brewer(palette="Dark2")
    g2<-ggplot(res2, aes(fill=variable, y=percent, x=timeframe)) + 
      geom_bar(position="stack", stat="identity") +
      labs(title="Variance Decomposition of CPI") + 
      scale_fill_brewer(palette="Dark2")
    g3<-ggplot(res3, aes(fill=variable, y=percent, x=timeframe)) + 
      geom_bar(position="stack", stat="identity") +
      labs(title="Variance Decomposition of EXR") + 
      scale_fill_brewer(palette="Dark2")
    g4<-ggplot(res4, aes(fill=variable, y=percent, x=timeframe)) + 
      geom_bar(position="stack", stat="identity") +
      labs(title="Variance Decomposition of M2") + 
      scale_fill_brewer(palette="Dark2")
    grid.arrange(g1, g2, g3, g4, nrow = 2)

The result is the plots of the variance decomposition:
![image](https://user-images.githubusercontent.com/88301287/169821318-c8098584-2eb1-4e2a-a33c-aa2c1d3cffef.png)

It is observed that the impact of M2 and exchange rate on GDP are negligible in a one-year timeframe. In the long run, it is observed that exchange rate causes a greater proportion of variance in GDP as compared to M2. 
Exchange rate starts to account for about 33.6% of shocks in GDP in the long run at the ten-year mark while M2 only accounts for about 14.2%.<br><br>
Exchange rate also explains a greater proportion of fluctuation in CPI as compared to M2 even in the short run even though its impact is relatively small. In the long run, exchange rate accounts for about 39.2% of shocks in inflation. 
On the other hand, M2 only accounts for about 19.0% which is significantly lower than that for exchange rate.<br><br>
In addition, majority of the variance in exchange rate is due to its own shocks regardless of timeframe whereas for M2, exchange rate shocks account for majority of the variance in M2.

<h1>Conclusion and Discussion</h1>
From our analysis, the impacts by exchange rate and monetary targeting are relatively similar and limited in the short run. However, in the long run, the impact of exchange rate on GDP is higher than that of M2 but comes at a cost of a greater impact on CPI, which indicates greater inflationary pressures. This makes exchange rate targeting a more effective policy over monetary targeting for achieving economic growth but not necessarily price stability. <br><br>
However, we believe that economic growth is a more important focus for Vietnam to progress to a more developed nation, and this is especially so with the Covid-19 pandemic that has caused Vietnam’s economic growth in 2020 to reach a 30-year low (CNA, 2020). Hence, a focus on economic growth in the long run will allow Vietnam to transit to a developed nation and recover from the effects of Covid-19. <br><br>
Furthermore, monetary targeting is also not as impactful as an independent policy compared to exchange rate targeting as a large portion of M2 shocks is due to exchange rate.<br><br>
  
The model however, faces limitations such as the possibility of omitted variable bias, so variables which we have omitted, such as interest rates set by the SBV, may have greater explanatory power than our current model. <br><br>
When testing out model for serial correlation, heteroskedasticity and normality, we notice that our model is not actually as robust as we thought. Hence, future works should look into a more robust model that considers more variables in the model, possibly included as exogeneous variables.<br><br>
  


