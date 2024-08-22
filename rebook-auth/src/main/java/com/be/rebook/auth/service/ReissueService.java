package com.be.rebook.auth.service;

import com.be.rebook.auth.dto.BasicUserInfoDTO;
import com.be.rebook.auth.entity.Members;
import com.be.rebook.auth.repository.MembersRepository;
import com.be.rebook.auth.repository.RefreshTokensRepository;
import com.be.rebook.auth.entity.RefreshTokens;
import com.be.rebook.common.exception.BaseException;
import com.be.rebook.common.exception.ErrorCode;
import com.be.rebook.auth.jwt.JWTUtil;
import com.be.rebook.auth.jwt.type.TokenCategory;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.Locale;


@Service
public class ReissueService {
    private final JWTUtil jwtUtil;
    private final RefreshTokensRepository refreshRepository;
    private final BCryptPasswordEncoder bCryptPasswordEncoder;
    private final MembersRepository membersRepository;

    public ReissueService(JWTUtil jwtUtil,
                          RefreshTokensRepository refreshTokensRepository,
                          MembersRepository membersRepository,
                          BCryptPasswordEncoder bCryptPasswordEncoder) {
        this.jwtUtil = jwtUtil;
        this.refreshRepository = refreshTokensRepository;
        this.membersRepository = membersRepository;
        this.bCryptPasswordEncoder = bCryptPasswordEncoder;
    }

    @Transactional
    public Members reissueUserPassword(HttpServletRequest request, BasicUserInfoDTO resetPasswordDTO) {
        String mailToken = null;

        if(request.getCookies().length == 0){
            throw new BaseException(ErrorCode.NO_TOKEN_CONTENT);
        }

        for(Cookie cookie : request.getCookies()){
            if(cookie.getName().equals(TokenCategory.MAILAUTH.getName())){
                mailToken = cookie.getValue();
                break;
            }
        }
        if (mailToken == null) {
            //NO_TOKEN_CONTENT
            throw new BaseException(ErrorCode.NO_TOKEN_CONTENT);
        }

        // expired check
        if(Boolean.TRUE.equals(jwtUtil.isExpired(mailToken))) {
            throw new BaseException(ErrorCode.EXPIRED_TOKEN);
        }

        String username = resetPasswordDTO.getUsername();
        String newPassword = resetPasswordDTO.getPassword();

        Members member = membersRepository.findByUsername(username)
                .orElseThrow(()->new BaseException(ErrorCode.NO_USER_INFO));

        member = member.toBuilder()
                .password(bCryptPasswordEncoder.encode(newPassword+username))
                .build();

        membersRepository.save(member);
        return Members.builder()
                .username(username)
                .build();
    }

    @Transactional
    public Members updateUserPassword(HttpServletRequest request, String passwordToUpdate) {
        String accessToken = request.getHeader("Authorization");

        if(accessToken == null){
            //NO_TOKEN_CONTENT
            throw new BaseException(ErrorCode.NO_TOKEN_CONTENT);
        }

        accessToken = accessToken.substring(7);

        if(Boolean.TRUE.equals(jwtUtil.isExpired(accessToken))){
            throw new BaseException(ErrorCode.EXPIRED_TOKEN);
        }

        String username = jwtUtil.getUsername(accessToken);

        Members member = membersRepository.findByUsername(username)
                .orElseThrow(()->new BaseException(ErrorCode.NO_USER_INFO));

        Members updatedMember = member
                .toBuilder()
                .password(bCryptPasswordEncoder.encode(passwordToUpdate+username))
                .build();

        membersRepository.save(updatedMember);
        return Members.builder()
                .username(username)
                .build();
    }

    public void deleteRefreshsOlderThanOneDay() {
        LocalDateTime cutoffDateTime = LocalDateTime.now().minusDays(1);
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("EEE MMM dd HH:mm:ss z yyyy", Locale.ENGLISH);
        String cutoff = cutoffDateTime.format(formatter);

        refreshRepository.deleteOldRefresh(cutoff);
    }

    public RefreshTokens reissueToken(HttpServletRequest request, HttpServletResponse response) {
        // get refresh token
        String refresh = null;

        if(request.getCookies().length == 0){
            throw new BaseException(ErrorCode.NO_TOKEN_CONTENT);
        }

        Cookie[] cookies = request.getCookies();
        for (Cookie cookie : cookies) {
            if (cookie.getName().equals(TokenCategory.REFRESH.getName())) {
                refresh = cookie.getValue();
            }
        }

        if (refresh == null) {
            //NO_TOKEN_CONTENT
            throw new BaseException(ErrorCode.NO_TOKEN_CONTENT);
        }

        // expired check
        if(Boolean.TRUE.equals(jwtUtil.isExpired(refresh))) {
            throw new BaseException(ErrorCode.EXPIRED_TOKEN);
        }

        // 토큰이 refresh인지 확인 (발급시 페이로드에 명시)
        TokenCategory category = jwtUtil.getCategory(refresh);

        if (category != TokenCategory.REFRESH) {
            //TOKEN_CATEGORY_INCORRECT
            throw new BaseException(ErrorCode.TOKEN_CATEGORY_INCORRECT);
        }

        //db에 리프레시 토큰이 저장되어 있는지 확인
        Boolean isExist = refreshRepository.existsByRefresh(refresh);
        if(Boolean.FALSE.equals(isExist)){
            //NO_TOKEN_CONTENT
            throw new BaseException(ErrorCode.NO_TOKEN_CONTENT);
        }

        String username = jwtUtil.getUsername(refresh);
        String role = jwtUtil.getRole(refresh);

        String newAccess = jwtUtil.createJwt(TokenCategory.ACCESS, username, role, TokenCategory.ACCESS.getExpiry());
        String newRefresh = jwtUtil.createJwt(TokenCategory.REFRESH, username, role, TokenCategory.REFRESH.getExpiry());

        //리프레쉬 토큰 저장 db에 기존의 리프레시 토큰 삭제 후 새 리프레시 토큰 저장
        refreshRepository.deleteByRefresh(refresh);
        addRefreshEntity(username,newRefresh,TokenCategory.REFRESH.getExpiry());

        // response
        response.setHeader(TokenCategory.ACCESS.getName(), newAccess);
        response.addCookie(createCookie(TokenCategory.REFRESH.getName(), newRefresh));
        return RefreshTokens.builder().username(username).refresh(refresh).build();
    }
    private Cookie createCookie(String key, String value) {

        Cookie cookie = new Cookie(key, value);
        cookie.setMaxAge(24*60*60);
        cookie.setHttpOnly(true);
        cookie.setPath("/");

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
