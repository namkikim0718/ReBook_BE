package com.be.rebook.auth.jwt;

import com.be.rebook.auth.jwt.type.TokenCategory;
import com.be.rebook.auth.utility.CookieUtil;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.web.filter.GenericFilterBean;

import java.io.IOException;

public class CustomLogoutFilter extends GenericFilterBean {
    private final CookieUtil cookieUtil;

    public CustomLogoutFilter(CookieUtil cookieUtil) {
        this.cookieUtil = cookieUtil;
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {
        doFilter((HttpServletRequest) request, (HttpServletResponse) response, chain);
    }

    private void doFilter(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws IOException, ServletException {

        // path and method verify
        String requestUri = request.getRequestURI();
        if (!requestUri.equals("/auth/signout")) {
            filterChain.doFilter(request, response);
            return;
        }

        String requestMethod = request.getMethod();
        if (!requestMethod.equals("POST")) {

            filterChain.doFilter(request, response);
            return;
        }

        // refresh token의 쿠이 expiry를 0 으로 설정해서 만료시킴
        Cookie cookie = cookieUtil.createCookie(TokenCategory.REFRESH.getName(), null, 0);

        response.addCookie(cookie);
        response.setStatus(HttpServletResponse.SC_OK);
    }
}
