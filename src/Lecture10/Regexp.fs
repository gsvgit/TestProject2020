module Regexp

open Automata

type Regexp<'t> =
    | RSmb of 't
    | Seq of Regexp<'t> * Regexp<'t>
    | Alt of Regexp<'t> * Regexp<'t>
    | Star of Regexp<'t>

let regexpToNFA regexp =
    let rec _go curFreeState curRegexp =
        match curRegexp with
        | RSmb s -> new NFA<_> (curFreeState, curFreeState + 1,
                                [ (curFreeState, Smb(s), curFreeState + 1) ])
        | Alt (l, r) ->
            let lAtm = _go curFreeState l
            let rAtm = _go (lAtm.FinalState + 1) r
            let newStart = rAtm.FinalState + 1
            let newFinal = rAtm.FinalState + 2
            let transitions =
                [
                    (newStart, Eps, lAtm.StartState)
                    (newStart, Eps, rAtm.StartState)
                    (lAtm.FinalState, Eps, newFinal)
                    (rAtm.FinalState, Eps, newFinal)
                ]
                @ rAtm.Transitions
                @ lAtm.Transitions
            new NFA<_> (newStart, newFinal, transitions)

        | Seq (l, r) ->
            let lAtm = _go curFreeState l
            let rAtm = _go (lAtm.FinalState + 1) r
            let newStart = rAtm.FinalState + 1
            let newFinal = rAtm.FinalState + 2
            let transitions =
                [
                    (newStart, Eps, lAtm.StartState)
                    (lAtm.FinalState, Eps, rAtm.StartState)
                    (rAtm.FinalState, Eps, newFinal)
                ]
                @ rAtm.Transitions
                @ lAtm.Transitions
            new NFA<_> (newStart, newFinal, transitions)

        | Star r ->
            let newAtm = _go curFreeState r
            let newStart = newAtm.FinalState + 1
            let newFinal = newAtm.FinalState + 2
            let transitions =
                [
                    (newStart, Eps, newAtm.StartState)
                    (newAtm.FinalState, Eps, newFinal)
                    (newStart, Eps, newFinal)
                    (newFinal, Eps, newStart)
                ]
                @ newAtm.Transitions
            new NFA<_> (newStart, newFinal, transitions)

    _go 0 regexp
