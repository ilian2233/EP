public class terminalSymbol{}

public class Parser {
    //abB
    public static terminalSymbol[] B(String input){
        if input[0]=='a' && input[1]=='b'{
            B(str.substring(2,input))
        }
    }

    //a(bB)*
    public static terminalSymbol[] B(String input){

        String[] arrOfStr = input.split("b");
        for (String a: arrOfStr)
            arr+=B(str.substring(1,a))

        arr = new terminalSymbol[]

        if input[0]=='a'{
            return arr
        }
        throw new Error()
    }

    //a(bB)?
    public static terminalSymbol[] B(String input){

        if !input.contains("b")
            throw new Error()


        arr = new terminalSymbol[]

        String[] arrOfStr = input.split("b");
        for (String a: arrOfStr)
            arr += B(str.substring(2,a))

        if input[0]=='a'{
            return arr
        }
        throw new Error()
    }

    //aA | bB | c(aC)*
    public static terminalSymbol[] B(String input){

        if input[0]=='a'{
            A(str.substring(1,input))
        }else if input[0]=='b'{
            B(str.substring(1,input))
        }else if input[0]=='c'{
            if(input[1]=='a'){
                C(str.substring(2,input))
            }else {
                throw new Error()
            }
        }else {
            throw new Error()
        }
    }
}