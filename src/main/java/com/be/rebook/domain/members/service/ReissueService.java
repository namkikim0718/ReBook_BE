package com.be.rebook.domain.members.service;

import com.be.rebook.domain.members.repository.RefreshTokensRepository;
import com.be.rebook.domain.members.entity.RefreshTokens;
import com.be.rebook.domain.members.jwt.JWTUtil;
import com.be.rebook.global.config.BaseResponse;
import com.be.rebook.global.exception.ErrorCode;
import io.jsonwebtoken.ExpiredJwtException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.Locale;

@Service
public class ReissueService {
    private final JWTUtil jwtUtil;
    private final RefreshTokensRepository refreshRepository;

    public ReissueService(JWTUtil jwtUtil,
                          RefreshTokensRepository refreshTokensRepository) {
        this.jwtUtil = jwtUtil;
        this.refreshRepository = refreshTokensRepository;
    }
    public void deleteRefreshsOlderThanOneDay() {
        LocalDateTime cutoffDateTime = LocalDateTime.now().minusDays(1);
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("EEE MMM dd HH:mm:ss z yyyy", Locale.ENGLISH);
        String cutoff = cutoffDateTime.format(formatter);

        refreshRepository.deleteOldRefresh(cutoff);
    }

    public BaseResponse<?> reissueToken(HttpServletRequest request, HttpServletResponse response) {
        // get refresh token
        String refresh = null;
        String refreshCategory = "refresh";
        String accessCategory = "access";
        Cookie[] cookies = request.getCookies();
        for (Cookie cookie : cookies) {
            if (cookie.getName().equals(refreshCategory)) {
                refresh = cookie.getValue();
            }
        }

        if (refresh == null) {
            //NO_TOKEN_CONTENT
            return new BaseResponse<>(ErrorCode.NO_TOKEN_CONTENT.getStatus(),
                    ErrorCode.NO_TOKEN_CONTENT.getStatus() + " failed",
                    ErrorCode.NO_TOKEN_CONTENT.getMessage(),
                    null);
        }

        // expired check
        try {
            jwtUtil.isExpired(refresh);
        } catch (ExpiredJwtException e) {
            //EXPIRED_TOKEN
            return new BaseResponse<>(ErrorCode.EXPIRED_TOKEN.getStatus(),
                    ErrorCode.EXPIRED_TOKEN.getStatus() + " failed",
                    ErrorCode.EXPIRED_TOKEN.getMessage(),
                    null);
        }

        // 토큰이 refresh인지 확인 (발급시 페이로드에 명시)
        String category = jwtUtil.getCategory(refresh);

        if (!category.equals(refreshCategory)) {
            //TOKEN_CATEGORY_INCORRECT
            return new BaseResponse<>(ErrorCode.TOKEN_CATEGORY_INCORRECT.getStatus(),
                    ErrorCode.TOKEN_CATEGORY_INCORRECT.getStatus()+" failed",
                    ErrorCode.TOKEN_CATEGORY_INCORRECT.getMessage(),
                    null);
        }

        //db에 리프레시 토큰이 저장되어 있는지 확인
        Boolean isExist = refreshRepository.existsByRefresh(refresh);
        if(Boolean.FALSE.equals(isExist)){
            //NO_TOKEN_CONTENT
            return new BaseResponse<>(ErrorCode.NO_TOKEN_CONTENT.getStatus(),
                    ErrorCode.NO_TOKEN_CONTENT.getStatus()+" failed",
                    ErrorCode.NO_TOKEN_CONTENT.getMessage(),
                    null);
        }

        String username = jwtUtil.getUsername(refresh);
        String role = jwtUtil.getRole(refresh);

        String newAccess = jwtUtil.createJwt(accessCategory, username, role, 600000L);
        String newRefresh = jwtUtil.createJwt(refreshCategory, username, role, 86400000L);

        //리프레쉬 토큰 저장 db에 기존의 리프레시 토큰 삭제 후 새 리프레시 토큰 저장
        refreshRepository.deleteByRefresh(refresh);
        addRefreshEntity(username,newRefresh,86400000L);
        //리프레시 토큰 저장소에서 기한이 지난 토큰 삭제
        //하루 지난 토큰은 삭제할 수 있게 스케줄링
        //-> RefreshDeleteDailyScheduler??

        // response
        response.setHeader(accessCategory, newAccess);
        response.addCookie(createCookie(refreshCategory, newRefresh));

        return new BaseResponse<>(null);
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
    private void addRefreshEntity(String username, String refresh, Long expriedMs){
        Date date = new Date(System.currentTimeMillis()+ expriedMs);

        RefreshTokens refreshTokens = RefreshTokens.builder()
                .username(username)
                .refresh(refresh)
                .expiration(date.toString())
                .build();

        refreshRepository.save(refreshTokens);
    }
}
