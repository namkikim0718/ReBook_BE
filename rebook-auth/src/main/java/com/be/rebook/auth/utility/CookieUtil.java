package com.be.rebook.auth.utility;

import org.springframework.stereotype.Component;

import com.be.rebook.auth.jwt.type.TokenCategory;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;

@Component
public class CookieUtil {

    public Cookie findCookieFromRequest(String key, HttpServletRequest request) {
        Cookie[] cookies = request.getCookies();

        if (cookies == null) {
            return null;
        }

        for (Cookie cookie : cookies) {
            if (cookie.getName().equals(key)) {
                return cookie;
            }
        }

        return null;
    }

    public Cookie createCookie(String key, String value, int maxAge) {
        Cookie cookie = new Cookie(key, value);
        cookie.setMaxAge(maxAge);
        cookie.setPath("/");
        // 자바스크립트로 해당 쿠키 접근 못하게
        cookie.setHttpOnly(true);
        return cookie;
    }
}
