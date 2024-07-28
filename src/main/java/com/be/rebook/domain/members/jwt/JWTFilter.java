package com.be.rebook.domain.members.jwt;

import com.be.rebook.domain.members.entity.Members;
import com.be.rebook.domain.members.dto.CustomUserDetails;
import com.be.rebook.global.exception.ErrorCode;
import io.jsonwebtoken.ExpiredJwtException;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;


public class JWTFilter extends OncePerRequestFilter {
    //필터를 검증할 메서드를 가져오려면 JWTUtil을 주입해야함
    private final JWTUtil jwtUtil;

    private static final Logger jwtLogger = LoggerFactory.getLogger(JWTFilter.class);


    public JWTFilter(JWTUtil jwtUtil){
        this.jwtUtil = jwtUtil;
    }
    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain)
            throws ServletException, IOException {

        String requestUri = request.getRequestURI();

        //로그인이나 관련 요청이 왔을땐 다음 필터로 넘겨서
        //재로그인 무한 루프를 방지함
        if (requestUri.matches("^\\/login(?:\\/.*)?$")) {

            filterChain.doFilter(request, response);
            return;
        }

        String accessToken = request.getHeader("access");
        //여기서 액세스 토큰 로깅했었는데 지움

        if (accessToken == null) {
            //권한이 필요 없는 경우도 있으니까
            filterChain.doFilter(request, response);

            return;
        }

        // 토큰 만료 여부 확인, 만료시 다음 필터로 넘기지 않음
        if(Boolean.TRUE.equals(jwtUtil.isExpired(accessToken))){
            jwtLogger.info("access token expired");
            //response status code = 401
            //프론트에서 알아야됨
            //EXPIRED_TOKEN
            response.setStatus(ErrorCode.EXPIRED_TOKEN.getStatus().value());
            return;
        }

        // 토큰이 access인지 확인 (발급시 페이로드에 명시)
        String category = jwtUtil.getCategory(accessToken);

        if (!category.equals("access")) {
            //TOKEN_CATEGORY_INCORRECT
            response.setStatus(ErrorCode.TOKEN_CATEGORY_INCORRECT.getStatus().value());
            return;
        }

        // username, role 값을 획득
        String username = jwtUtil.getUsername(accessToken);
        String role = jwtUtil.getRole(accessToken);

        Members members = Members.builder()
                .username(username)
                .role(role)
                .build();

        CustomUserDetails customUserDetails = new CustomUserDetails(members);

        Authentication authToken = new UsernamePasswordAuthenticationToken(customUserDetails, null, customUserDetails.getAuthorities());

        SecurityContextHolder.getContext().setAuthentication(authToken);
        filterChain.doFilter(request, response);

    }
}
