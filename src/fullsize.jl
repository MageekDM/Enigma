function get_perm(str::String)
    p = Array(Int, length(str))
    for (i,c) in enumerate(str)
        p[i] = char2ind(c)
    end
    p
end

const ENIGMA_ROTORS = [
    Rotor(get_perm("EKMFLGDQVZNTOWYHXUSPAIBRCJ"), Int[2]), #char2ind('Q')]), # I
    Rotor(get_perm("AJDKSIRUXBLHWTMCQGZNPYFVOE"), Int[5]), # char2ind('E')]), # II
    Rotor(get_perm("BDFHJLCPRTXVZNYEIWGAKMUSQO"), Int[char2ind('V')]), # III
    Rotor(get_perm("ESOVPZJAYQUIRHXLNFTGKDCMWB"), Int[char2ind('J')]), # IV
    Rotor(get_perm("VZBRGITYUPSDNHLXAWMJQOFECK"), Int[char2ind('Z')]), # V
    Rotor(get_perm("JPGVOUMFYQBENHZRDKASXLICTW"), Int[char2ind('Z'), char2ind('M')]), # VI
    Rotor(get_perm("NZJHGRCXMYSWBOUFAIVLPEKQDT"), Int[char2ind('Z'), char2ind('M')]), # VII
    Rotor(get_perm("FKQHTLXOCBJSPDZRAMEWNIUYGV"), Int[char2ind('Z'), char2ind('M')]), # VIII
]

const ENIGMA_REFLECTORS = [
    Reflector(get_perm("YRUHQSLDPXNGOKMIEBFZCWVJAT")), # b
    Reflector(get_perm("FVPJIAOYEDRZXWGCTKUQSBNMHL")), # c
    Reflector(get_perm("ENKQAUYWJICOPBLMDXZVFTHRGS")), # b - thin
    Reflector(get_perm("RDOBJNTKVEHMLFCWZAXGYIPSUQ")), # c - thin
]

function get_M3_enigma(
    ref::Char, # 'B' or 'C'
    wheels::Tuple{Int, Int, Int},
    rotor_pos::String, # the starting rotor positions, ex: "DVM"
    plugboard_str::String=""; # ex: "BI GP HK LD MO RT VS WZ XN YJ"
    emulate_double_stepping::Bool=true,
    )

    if ref == 'B'
        reflector = ENIGMA_REFLECTORS[1]
    elseif ref == 'C'
        reflector = ENIGMA_REFLECTORS[2]
    else
        error("unknown reflector $ref")
    end

    rotors = ENIGMA_ROTORS[[wheels[3], wheels[2], wheels[1]]]
    rotors[1].rotor_position = rotor_pos[3]-'A'
    rotors[2].rotor_position = rotor_pos[2]-'A'
    rotors[3].rotor_position = rotor_pos[1]-'A'

    plugboard = Plugboard(collect(1:26))
    plugboard_str = replace(plugboard_str, " ", "")
    i = 1
    while i < length(plugboard_str)
        a = char2ind(plugboard_str[i])
        b = char2ind(plugboard_str[i+1])
        plugboard.perm[a] = b
        plugboard.perm[b] = a
        i += 2
    end

    alphabet = collect('A':'Z')
    Enigma(plugboard, rotors, reflector, alphabet, emulate_double_stepping)
end

function encode!(enigma::Enigma, input::String)
    output = ""
    for c in input
        output = output * string(ind2char(encode!(enigma, char2ind(c))))
    end
    output
end

let
    enigma = get_M3_enigma('B', (1,2,3), "AAA")
    input =  "THEQ"*"UICK"*"BROW"*"NFOX"*"JUMP"*"EDOV"*"ERTH"*"ELAZ"*"YDOG"
    output = "OPCI"*"LLAZ"*"FXLQ"*"TDNL"*"GGLE"*"YOHH"*"CJGA"*"XWTW"*"AMBH"
    @assert encode!(enigma, input) == output

    enigma = get_M3_enigma('B', (1,2,3), "AAA")
    @assert encode!(enigma, output) == input

    enigma = get_M3_enigma('B', (1,2,3), "AAA")
    input2 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    output = "BDZGOWCXLTKSBTMCDLPBMUQOFXYHCXTGYJFLINHNXSHIUNTHEORXPQPKOVHCBUBTZSZSOOSTGOTFSODBBZZLXLCYZXIFGWFDZEEQIBMGFJBWZFCKPFMGBXQCIVIBBRNCOCJUVYDKMVJPFMDRMTGLWFOZLXGJEYYQPVPBWNCKVKLZTCBDLDCTSNRCOOVPTGBVBBISGJSOYHDENCTNUUKCUGHREVWBDJCTQXXOGLEBZMDBRZOSXDTZSZBGDCFPRBZYQGSNCCHGYEWOHVJBYZGKDGYNNEUJIWCTYCYTUUMBOYVUNNQUKKSOBSCORSUOSCNVROQLHEUDSUKYMIGIBSXPIHNTUVGGHIFQTGZXLGYQCNVNSRCLVPYOSVRBKCEXRNLGDYWEBFXIVKKTUGKPVMZOTUOGMHHZDREKJHLEFKKPOXLWBWVBYUKDTQUHDQTREVRQJMQWNDOVWLJHCCXCFXRPPXMSJEZCJUFTBRZZMCSSNJNYLCGLOYCITVYQXPDIYFGEFYVXSXHKEGXKMMDSWBCYRKIZOCGMFDDTMWZTLSSFLJMOOLUUQJMIJSCIQVRUISTLTGNCLGKIKTZHRXENRXJHYZTLXICWWMYWXDYIBLERBFLWJQYWONGIQQCUUQTPPHBIEHTUVGCEGPEYMWICGKWJCUFKLUIDMJDIVPJDM"
    pred = replace(encode!(enigma, input2), " ", "")
    @assert pred == output

    enigma = get_M3_enigma('B', (3,5,8), "MCJ")
    output2 = "JFHJ"*"BXOD"*"IIZK"*"CHJU"*"TGOL"*"FIYX"*"RSLF"*"KKPO"*"JVRO"
    @assert encode!(enigma, input) == output2
    enigma = get_M3_enigma('B', (3,5,8), "MCJ")
    @assert encode!(enigma, output2) == input

    enigma = get_M3_enigma('C', (8,2,4), "PXM", "AB CY ET FQ HZ LG MJ OK US WX")
    output3 = "GXUH"*"KCKB"*"QJKL"*"QJUZ"*"SFNU"*"BQBX"*"HFYM"*"DIRE"*"PIRN"
    @assert encode!(enigma, input) == output3
    enigma = get_M3_enigma('C', (8,2,4), "PXM", "AB CY ET FQ HZ LG MJ OK US WX")
    @assert encode!(enigma, output3) == input
end