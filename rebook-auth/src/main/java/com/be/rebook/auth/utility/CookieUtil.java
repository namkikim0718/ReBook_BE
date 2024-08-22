package com.be.rebook.auth.utility;

import org.springframework.stereotype.Component;

import com.be.rebook.auth.jwt.type.TokenCategory;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;

@Component
public class CookieUtil {

    public Cookie findCookieFromRequest(String key, HttpServletRequest request) {
        Cookie[] cookies = request.getCookies();

        for (Cookie cookie : cookies) {
            if (cookie.getName().equals(TokenCategory.REFRESH.getName())) {
                return cookie;
            }
        }

        return null;

        // String refresh = null;

        // if (request.getCookies().length == 0) {
        // throw new BaseException(ErrorCode.NO_TOKEN_CONTENT);
        // }

        // Cookie[] cookies = request.getCookies();
        // for (Cookie cookie : cookies) {
        // if (cookie.getName().equals(TokenCategory.REFRESH.getName())) {
        // refresh = cookie.getValue();
        // }
        // }
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
