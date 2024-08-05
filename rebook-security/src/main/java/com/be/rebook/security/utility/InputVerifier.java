package com.be.rebook.security.utility;

public class InputVerifier {
    private static final String[] SQL_INJECTION_CHARS = {"'", "\"", ";", "--", "\\"};

    private InputVerifier(){}
    public static String verifyForSQL(String input) {
        if (input == null) {
            return null;
        }

        for (String specialChar : SQL_INJECTION_CHARS) {
            input = input.replace(specialChar, "\\" + specialChar);
        }
        return input;
    }

    public static String verifyForXSS(String input) {
        if (input == null) {
            return null;
        }

        input = input.replace("&", "&amp;");
        input = input.replace("<", "&lt;");
        input = input.replace(">", "&gt;");
        input = input.replace("\"", "&quot;");
        input = input.replace("'", "&#x27;");
        input = input.replace("/", "&#x2F;");
        return input;
    }

    //아이디 생성 조건
    //25자 이하
    //특수문자 안됨
    //한글안됨
    public static Boolean checkUsernameCharacters(String input){
        int inputLength = input.length();
        if(inputLength == 0)
            return false;
        Boolean specialCharCheck = input.matches(".*[^a-zA-Z0-9].*");
        return inputLength <= 25 && !Boolean.TRUE.equals(specialCharCheck);
    }

    //비밀번호 생성 조건
    // 8자 이하 => 너무짧음
    // sql injection, xss 공격 방어 필터링
    //
    public static Boolean checkPasswordCharacters(String input){
        int inputLength = input.length();
        return inputLength >= 8;
    }

    // 통합 필터링
    public static String sanitizeInput(String input) {
        return verifyForSQL(verifyForXSS(input));
    }

}
