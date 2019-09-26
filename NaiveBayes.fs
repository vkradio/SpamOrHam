namespace NaiveBayes

module Classifier =
    
    type Token = string
    type Tokenizer = string -> Token Set
    type TokenizedDoc = Token Set
    type DocsGroup = { Proportion:float; TokenFrequencies:Map<Token, float> }

    // Calculate logarithm of token's Laplace score, if any.
    // Zero, if none.
    let tokenScore (group: DocsGroup) (token: Token) =
        if group.TokenFrequencies.ContainsKey token
        then log group.TokenFrequencies.[token]
        else 0.0


    // For each token in sms ("document"), calculate logarithm of it's score,
    // then sum all these logarithms
    // then add logarithm of group's proportion
    let score (document: TokenizedDoc) (group: DocsGroup) =
        // Curried function: attach group, wait for token
        let scoreToken = tokenScore group
        // Result: float
        log group.Proportion + (document |> Seq.sumBy scoreToken)

    
    let classify (groups: (_ * DocsGroup)[])
                 (tokenizer: Tokenizer)
                 (txt: string) =
        // Split sms text to tokens
        let tokenized = tokenizer txt

        // For each labeled group of anayzed smses, 
        groups
        |> Array.maxBy (fun (_, group) -> score tokenized group)
        |> fst


    let proportion count total = float count / float total
    let laplace count total = float (count + 1) / float (total + 1)

    // Number of sms messages in spam|ham group, containing given token at least once
    let countIn (group:TokenizedDoc seq) (token:Token) =
        group
        |> Seq.filter (Set.contains token)
        |> Seq.length

    
    // For each token in classificationTokens set, calculate Laplace score relative to sms group.
    // Then also add group proportion relative to all smses.
    let analyze (group: TokenizedDoc seq)           // All documents (aka sms messages) in one of the group (either "Spam", or "Ham")
                (totalDocs: int)                    // Number of all documents in all groups
                (classificationTokens: Token Set) = // All (?) tokens

        // Value int:
        // Number of messages in spam|ham group
        let groupSize = group |> Seq.length

        // Function (Token) -> float
        // Laplace-smoothened score (number of smses with given token in group / number of all smses in group)
        let score token =
            let count = countIn group token
            laplace count groupSize

        // Value:
        // Tuples of all tokens with corresponding calculated Laplace scores
        let scoredTokens =
            classificationTokens
            |> Set.map (fun token -> token, score token)
            |> Map.ofSeq

        // Value float:
        // Number of smses in group / total number of smses
        let groupProportion = proportion groupSize totalDocs

        // Result record:
        {
            Proportion = groupProportion    // Weight of group relative to total number of smses
            TokenFrequencies = scoredTokens // Tuples of Laplace-scored tokens
        }


    // Produces two array entries, one per group (spam|ham).
    // Inside each of group, produces labeled DocsGroup record containing
    // a) group proportion relative to the group, and
    // b) Laplace score of each classification (learning) token relative to that group.
    let learn (docs: (_ * string)[])                // All smses, each tagged by group name (spam|ham)
              (tokenizer: Tokenizer)                // Function (sms string) -> set of tokens
              (classificationTokens: Token Set) =   // Set of tokens (?)

        // Value int: Total number of smses
        let total = docs.Length

        docs
        // 1. Split each sms string to set of tokens
        |> Array.map (fun (label, doc) -> label, tokenizer doc)
        // 2. Split tokenized smses to (two) groups by label (spam|ham)
        |> Seq.groupBy fst
        // 3. Exclude excess grouping root
        |> Seq.map (fun (label, group) -> label, group |> Seq.map snd)
        // 4. For each group, calculate group's proportion, and calculate Laplace score of each token relative
        //    to smses in this group
        |> Seq.map (fun (label, group) -> label, analyze group total classificationTokens)
        // 5. Just finalize resulting seq to array
        |> Seq.toArray


    // Curried function:
    // 1. Based on classificationTokens, create sms groups with Laplace scores and group proportion;
    // 2. Return curried function with groups and tokenized, awaiting particular sms.
    // Result of executing this curried function with attached sms will be label spam or ham.
    let train (docs: (_ * string)[])
              (tokenizer: Tokenizer)
              (classificationTokens: Token Set) =

        let groups = learn docs tokenizer classificationTokens

        let classifier = classify groups tokenizer

        classifier
