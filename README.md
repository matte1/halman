## Halman

My simple attempt at writing a EKF/UKF in Haskell.

#### Examples

1. stack build
2. stack exec CannonBall
3. open CannonBall.svg

###### CannonBall

Simulates a cannon ball shot with Newtonian equations of motion. The nonlinearity here is added to the measurement values. Note I think I have an issue with the UKF as the estimate seems to diverge after a long period. Looks like there might be some numerical instability with cholesky decomposition.

###### Van Der Pol

Simulates the Van Der Pol oscillator. WGN is added to both the sensor and the process
