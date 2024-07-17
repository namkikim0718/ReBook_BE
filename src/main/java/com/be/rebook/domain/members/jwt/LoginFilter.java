package com.be.rebook.domain.members.jwt;

import com.be.rebook.domain.members.entity.RefreshTokens;
import com.be.rebook.domain.members.repository.RefreshTokensRepository;
import jakarta.servlet.FilterChain;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpStatus;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

import java.util.Collection;
import java.util.Date;
import java.util.Iterator;

public class LoginFilter extends UsernamePasswordAuthenticationFilter {

    private final AuthenticationManager authenticationManager;

    //jwt 토큰을 사용해야하므로 주입
    private final JWTUtil jwtUtil;

    private RefreshTokensRepository refreshTokensRepository;
    public LoginFilter(AuthenticationManager authenticationManager,
                       JWTUtil jwtUtil,
                       RefreshTokensRepository refreshTokensRepository){
        this.authenticationManager = authenticationManager;
        this.jwtUtil = jwtUtil;
        this.refreshTokensRepository = refreshTokensRepository;
    }
    @Override
    public Authentication attemptAuthentication(
            HttpServletRequest request, HttpServletResponse response)
            throws AuthenticationException {
        String username = obtainUsername(request);
        //솔팅 : 유저 아이디를 각각 붙여서 하나가 뚫리더라도 다른 멤버는 안전하게
        String password = obtainPassword(request)+username;


        System.out.println("attemptAuthentication username : " + username);

        UsernamePasswordAuthenticationToken authToken =
                new UsernamePasswordAuthenticationToken(username,password,null);


        System.out.println("attemptAuthentication "+ SecurityContextHolder.getContext().getAuthentication());

        return authenticationManager.authenticate(authToken);
    }

    @Override
    protected void successfulAuthentication(
            HttpServletRequest request, HttpServletResponse response,
            FilterChain chain, Authentication authentication){

        String username = authentication.getName();

        Collection<? extends GrantedAuthority> authorities = authentication.getAuthorities();
        Iterator<? extends  GrantedAuthority> iterator = authorities.iterator();
        GrantedAuthority auth = iterator.next();

        String role = auth.getAuthority();

        String access = jwtUtil.createJwt("access", username, role, 600000L);
        String refresh = jwtUtil.createJwt("refresh", username, role, 86400000L);

        addRefreshEntity(username, refresh, 86400000L);

        response.setHeader("access", access);
        response.addCookie(createCookie("refresh", refresh));
        response.setStatus(HttpStatus.OK.value());

        System.out.println("successfulAuthentication "+SecurityContextHolder.getContext().getAuthentication());
    }

    @Override
    protected void unsuccessfulAuthentication(
            HttpServletRequest request, HttpServletResponse response,
            AuthenticationException failed){
        System.out.println("로그인 실패 ");
        response.setStatus(401);
    }

    private Cookie createCookie(String key, String value) {

        Cookie cookie = new Cookie(key, value);
        //한시간짜리
        cookie.setMaxAge(24*60*60);
        //https에서만 되게 하는 옵션
        //cookie.setSecure(true);
        //쿠키가 적용될 범위
        //cookie.setPath("/");
        //자바스크립트로 해당 쿠키 접근 못하게 하는 옵션
        cookie.setHttpOnly(true);

        return cookie;
    }

    private void addRefreshEntity(String username, String refresh, Long expiredMs) {

        Date date = new Date(System.currentTimeMillis() + expiredMs);

        RefreshTokens refreshTokens = new RefreshTokens();
        refreshTokens.setUsername(username);
        refreshTokens.setRefresh(refresh);
        refreshTokens.setExpiration(date.toString());

        refreshTokensRepository.save(refreshTokens);
    }
}
