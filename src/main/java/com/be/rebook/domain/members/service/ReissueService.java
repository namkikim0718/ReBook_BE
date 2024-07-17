package com.be.rebook.domain.members.service;

import com.be.rebook.domain.members.repository.RefreshTokensRepository;
import com.be.rebook.domain.members.entity.RefreshTokens;
import com.be.rebook.domain.members.jwt.JWTUtil;
import io.jsonwebtoken.ExpiredJwtException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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

    public ResponseEntity<?> reissueToken(HttpServletRequest request, HttpServletResponse response) {
        // get refresh token
        String refresh = null;
        Cookie[] cookies = request.getCookies();
        for (Cookie cookie : cookies) {
            if (cookie.getName().equals("refresh")) {
                refresh = cookie.getValue();
            }
        }

        if (refresh == null) {
            // response status code
            return new ResponseEntity<>("refresh token null", HttpStatus.BAD_REQUEST);
        }

        // expired check
        try {
            jwtUtil.isExpired(refresh);
        } catch (ExpiredJwtException e) {
            // response status code
            return new ResponseEntity<>("refresh token expired", HttpStatus.BAD_REQUEST);
        }

        // 토큰이 refresh인지 확인 (발급시 페이로드에 명시)
        String category = jwtUtil.getCategory(refresh);

        if (!category.equals("refresh")) {
            // response status code
            return new ResponseEntity<>("invalid refresh token", HttpStatus.BAD_REQUEST);
        }

        //db에 리프레시 토큰이 저장되어 있는지 확인
        Boolean isExist = refreshRepository.existsByRefresh(refresh);
        if(!isExist){
            //response body
            return new ResponseEntity<>("invaild refresh token", HttpStatus.BAD_REQUEST);
        }

        String username = jwtUtil.getUsername(refresh);
        String role = jwtUtil.getRole(refresh);

        String newAccess = jwtUtil.createJwt("access", username, role, 600000L);
        String newRefresh = jwtUtil.createJwt("refresh", username, role, 86400000L);

        //리프레쉬 토큰 저장 db에 기존의 리프레시 토큰 삭제 후 새 리프레시 토큰 저장
        refreshRepository.deleteByRefresh(refresh);
        addRefreshEntity(username,newRefresh,86400000L);
        //리프레시 토큰 저장소에서 기한이 지난 토큰 삭제
        //하루 지난 토큰은 삭제할 수 있게 스케줄링
        //-> RefreshDeleteDailyScheduler??

        // response
        response.setHeader("access", newAccess);
        response.addCookie(createCookie("refresh", newRefresh));

        return new ResponseEntity<>(HttpStatus.OK);
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
        RefreshTokens refreshTokens = new RefreshTokens();
        refreshTokens.setUsername(username);
        refreshTokens.setRefresh(refresh);
        refreshTokens.setExpiration(date.toString());

        refreshRepository.save(refreshTokens);
    }
}
