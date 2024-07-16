package com.be.rebook.domain.members.service;

import com.be.rebook.domain.members.dto.UpdateDTO;
import com.be.rebook.domain.members.entity.Members;
import com.be.rebook.domain.members.entity.RefreshTokens;
import com.be.rebook.domain.members.jwt.JWTUtil;
import com.be.rebook.domain.members.repository.MembersRepository;
import com.be.rebook.domain.members.repository.RefreshTokensRepository;
import io.jsonwebtoken.ExpiredJwtException;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class MemberService {

    private MembersRepository membersRepository;
    private JWTUtil jwtUtil;

    private RefreshTokensRepository refreshTokensRepository;

    public MemberService(MembersRepository membersRepository,
                         JWTUtil jwtUtil,
                         RefreshTokensRepository refreshTokensRepository){
        this.membersRepository = membersRepository;
        this.jwtUtil = jwtUtil;
        this.refreshTokensRepository = refreshTokensRepository;
    }

    public Members getMemberByUsername(String username) {
        return membersRepository.findByUsername(username);
    }

    public ResponseEntity<Members> updateUser(String token, UpdateDTO membersUpdateDTO) {
        try {
            jwtUtil.isExpired(token);
        } catch (ExpiredJwtException e) {
            System.out.println("회원 정보 업데이트 오류 : 토큰 만료됨");
            return ResponseEntity.status(HttpServletResponse.SC_BAD_REQUEST).build();
        }

        String username = jwtUtil.getUsername(token);

        if (membersRepository.existsByUsername(username)) {
            Members member = membersRepository.findByUsername(username);

            if (membersUpdateDTO.getMemberName() != null) {
                member.setMemberName(membersUpdateDTO.getMemberName());
            }

            if (membersUpdateDTO.getUniversity() != null) {
                member.setUniversity(membersUpdateDTO.getUniversity());
            }

            Members updatedMember = membersRepository.save(member);
            return ResponseEntity.ok(updatedMember);
        } else {
            System.out.println("회원 정보 업데이트 오류 : 해당 유저 없음");
            return ResponseEntity.notFound().build();
        }
    }

    // TroubleShooting
    //403 오류 해결
    // -> SecurityContextHolder에 토큰 생성할때 등록
    // -> isExpired에서 현재 날짜로 그 전에 토큰의 유효기간이 끝나는게 아니라 현재 날짜 + 유효기간으로 판단.
    public ResponseEntity<Void> deleteUser(String token) {
        try {
            jwtUtil.isExpired(token);
        } catch (ExpiredJwtException e) {
            System.out.println("회원 탈퇴 오류 : 토큰 만료됨");
            return ResponseEntity.status(HttpServletResponse.SC_BAD_REQUEST).build();
        }

        String username = jwtUtil.getUsername(token);

        if (membersRepository.existsByUsername(username)) {
            Members member = membersRepository.findByUsername(username);
            List<RefreshTokens> refreshTokens = refreshTokensRepository.findByUsername(username);
            for(RefreshTokens tokenToDelete : refreshTokens){
                refreshTokensRepository.delete(tokenToDelete);
            }
            membersRepository.delete(member);
            return ResponseEntity.ok().build();
        } else {
            System.out.println("회원 탈퇴 오류 : 유저 없음 ");
            return ResponseEntity.notFound().build();
        }
    }
}

