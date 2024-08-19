package com.be.rebook.auth.utility;

import org.springframework.stereotype.Component;

import jakarta.servlet.http.Cookie;

@Component
public class CookieUtil {


    public Cookie createCookie(String key, String value, int maxAge) {
        Cookie cookie = new Cookie(key, value);
        cookie.setMaxAge(maxAge);
        cookie.setPath("/");
        //자바스크립트로 해당 쿠키 접근 못하게
        cookie.setHttpOnly(true);

        return cookie;
    }
}
