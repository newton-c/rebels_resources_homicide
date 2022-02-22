
library(DiagrammeR)


grViz("
    digraph {
        graph [ranksep = 0.2]
        node [shape = plaintext]
            R [label = 'Illicit\nResources']
            H [label = 'Homicides']
        edge [minlen = 6]
            R -> H
        {rank = same; R; H}
}
")


grViz("
    digraph {
        graph [ranksep = 0.2]
        node [shape = plaintext]
            R [label = 'Illicit\nResources']
            V [label = 'Violence']
            H [label = 'Homicides']
        edge [minlen = 6]
            R -> H
            R -> V
            V -> H

        {rank = same; R; V}
}
")


grViz("
    digraph {
        graph [ranksep = 0.2]
        node [shape = plaintext]
            R [label = 'Illicit\nResources']
            G [label = 'Government\nWeakness']
            V [label = 'Violence']
            H [label = 'Homicides']
        edge [minlen = 6]
            R -> H
            R -> V
            G -> R
            G -> H
            V -> H

        {rank = same; R; V}
        {rank = same; G; H}
}
")
