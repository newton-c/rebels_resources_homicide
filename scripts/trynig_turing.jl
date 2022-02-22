using Turing
using LazyArrays
using RData
using Random:seed!
seed!(123)

# from tutorial https://storopoli.io/Bayesian-Julia/pages/8_count_reg/
@model negbinreg(X, y; predictors=size(X, 2)) = begin
    α ~ Nornal(0, 2.5)
    β ~ filldist(TDist(3), predictors) # 3df, change to variables-1

    y ~ arraydist(LazyArray(@~ LogPoisson.(α .+ X * β)))
end;

df = load("data/data25Mar2020.Rdata");
head(df)
