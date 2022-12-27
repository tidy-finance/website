# Proofs

## Optimal portfolio choice

### The minimum variance portfolio

The minimum variance portfolio weights are given by the solution to $$\omega_\text{mvp} = \arg\min w'\Sigma w \text{ s.t. } \iota'w= 1,$$ where $\iota$ is an $(N \times 1)$ vector of ones. The Lagrangian reads $$ \mathcal{L}(\omega) = w'\Sigma w - \lambda(w'\iota - 1).$$ We can solve the first-order conditions of the Lagrangian equation: $$
\begin{aligned}
& \frac{\partial\mathcal{L}(\omega)}{\partial\omega} = 0 \Leftrightarrow 2\Sigma w = \lambda\iota \Rightarrow \omega = \frac{\lambda}{2}\Sigma^{-1}\iota \\ \end{aligned}
$$ Next, the constraint that weights have to sum up to one delivers: $1 = \iota'\omega = \frac{\lambda}{2}\iota'\Sigma^{-1}\iota \Rightarrow \lambda = \frac{2}{\iota'\Sigma^{-1}\iota}.$ Finally, plug-in the derived value of $\lambda$ to get $$
\begin{aligned}
\omega_\text{mvp} = \frac{\Sigma^{-1}\iota}{\iota'\Sigma^{-1}\iota}.
\end{aligned}
$$ 

### The efficient portfolio

Consider an investor who aims to achieve minimum variance *given a desired expected return* $\bar{\mu}$, that is: $$\omega_\text{eff}\left(\bar{\mu}\right) = \arg\min w'\Sigma w \text{ s.t. } \iota'w = 1 \text{ and } \omega'\mu \geq \bar{\mu}.$$ The Lagrangian reads $$ \mathcal{L}(\omega) = w'\Sigma w - \lambda(w'\iota - 1) - \tilde{\lambda}(\omega'\mu - \bar{\mu}). $$ We can solve the first-order conditions to get $$
\begin{aligned}
2\Sigma w &= \lambda\iota + \tilde\lambda \mu\\
\Rightarrow\omega &= \frac{\lambda}{2}\Sigma^{-1}\iota + \frac{\tilde\lambda}{2}\Sigma^{-1}\mu.
\end{aligned}
$$

Next, the two constraints ($w'\iota = 1 \text{ and } \omega'\mu \geq \bar{\mu}$) imply $$
\begin{aligned}
1 &= \iota'\omega = \frac{\lambda}{2}\underbrace{\iota'\Sigma^{-1}\iota}_{C} + \frac{\tilde\lambda}{2}\underbrace{\iota'\Sigma^{-1}\mu}_D\\
\Rightarrow \lambda&= \frac{2 - \tilde\lambda D}{C}\\
\bar\mu &= \mu'\omega = \frac{\lambda}{2}\underbrace{\mu'\Sigma^{-1}\iota}_{D} + \frac{\tilde\lambda}{2}\underbrace{\mu'\Sigma^{-1}\mu}_E = \frac{1}{2}\left(\frac{2 - \tilde\lambda D}{C}\right)D+\frac{\tilde\lambda}{2}E  \\&=\frac{D}{C}+\frac{\tilde\lambda}{2}\left(E - \frac{D^2}{C}\right)\\
\Rightarrow \tilde\lambda &= 2\frac{\bar\mu - D/C}{E-D^2/C}.
\end{aligned}
$$ As a result, the efficient portfolio weight takes the form (for $\bar{\mu} \geq D/C = \mu'\omega_\text{mvp}$) $$w_\text{eff}\left(\bar\mu\right) = \omega_\text{mvp} + \frac{\tilde\lambda}{2}\left(\Sigma^{-1}\mu -\frac{D}{C}\Sigma^{-1}\iota \right).$$ Thus, the efficient portfolio allocates wealth in the minimum variance portfolio $\omega_\text{mvp}$ and a levered (self-financing) portfolio to increase the expected return.

Note that the portfolio weights sum up to one as $$\iota'\left(\Sigma^{-1}\mu -\frac{D}{C}\Sigma^{-1}\iota \right) = D - D = 0\text{ so }\iota'\omega_\text{eff} = \iota'\omega_\text{mvp} = 1.$$ Finally, the expected return of the efficient portfolio is $$\mu'\omega_\text{eff} = \frac{D}{C} + \bar\mu - \frac{D}{C} = \bar\mu.$$

### Equivalence between Certainty equivalent maximization and minimum variance optimization

We argue that an investor with a quadratic utility function with certainty equivalent $$\max_w CE(w) = \omega'\mu - \frac{\gamma}{2} \omega'\Sigma \omega \text{ s.t. } \iota'\omega = 1$$ faces an equivalent optimization problem to a framework where portfolio weights are chosen with the aim to minimize volatility given a pre-specified level or expected returns $$\min_w \omega'\Sigma \omega \text{ s.t. } \omega'\mu = \bar\mu \text{ and } \iota'\omega = 1.$$ Note the difference: In the first case, the investor has a (known) risk aversion $\gamma$ which determines her optimal balance between risk ($\omega'\Sigma\omega)$ and return ($\mu'\omega$). In the second case, the investor has a target return she wants to achieve while minimizing the volatility. Intuitively, both approaches are closely connected if we consider that the risk aversion $\gamma$ determines the desirable return $\bar\mu$. More risk averse investors (higher $\gamma$) will chose a lower target return to keep their volatility level down. The efficient frontier then spans all possible portfolios depending on the risk aversion $\gamma$, starting from the minimum variance portfolio ($\gamma = \infty$).

To proof this equivalence, consider first the optimal portfolio weights for a certainty equivalent maximizing investor. The first order condition reads $$
\begin{aligned}
\mu - \lambda \iota &= \gamma \Sigma \omega \\
\Leftrightarrow \omega &= \frac{1}{\gamma}\Sigma^{-1}\left(\mu - \lambda\iota\right)
\end{aligned}
$$ Next, we make use of the constraint $\iota'\omega = 1$. $$
\begin{aligned}
\iota'\omega &= 1 = \frac{1}{\gamma}\left(\iota'\Sigma^{-1}\mu - \lambda\iota'\Sigma^{-1}\iota\right)\\
\Rightarrow \lambda &= \frac{1}{\iota'\Sigma^{-1}\iota}\left(\iota'\Sigma^{-1}\mu - \gamma \right).
\end{aligned}
$$ Plug-in the value of $\lambda$ reveals the desired portfolio for an investor with risk aversion $\gamma$. $$
\begin{aligned}
\omega &= \frac{1}{\gamma}\Sigma^{-1}\left(\mu - \frac{1}{\iota'\Sigma^{-1}\iota}\left(\iota'\Sigma^{-1}\mu - \gamma \right)\right) \\
\Rightarrow \omega &= \frac{\Sigma^{-1}\iota}{\iota'\Sigma^{-1}\iota} + \frac{1}{\gamma}\left(\Sigma^{-1} - \frac{\Sigma^{-1}\iota}{\iota'\Sigma^{-1}\iota}\iota'\Sigma^{-1}\right)\mu\\
&= \omega_\text{mvp} + \frac{1}{\gamma}\left(\Sigma^{-1}\mu - \frac{\iota'\Sigma^{-1}\mu}{\iota'\Sigma^{-1}\iota}\Sigma^{-1}\iota\right).
\end{aligned}
$$ The resulting weights correspond to the efficient portfolio with desired return $\bar r$ such that (in the notation of book) $$\frac{1}{\gamma} = \frac{\tilde\lambda}{2} = \frac{\bar\mu - D/C}{E - D^2/C}$$ which implies that the desired return is just $$\bar\mu = \frac{D}{C} + \frac{1}{\gamma}\left({E - D^2/C}\right)$$ which is $\frac{D}{C} = \mu'\omega_\text{mvp}$ for $\gamma\rightarrow \infty$ as expected. For instance, letting $\gamma \rightarrow \infty$ implies $\bar\mu = \frac{D}{C} = \omega_\text{mvp}'\mu$.
