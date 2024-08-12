package com.be.rebook.members.controller;

import com.be.rebook.members.entity.Members;
import com.be.rebook.members.service.MemberService;
import com.be.rebook.members.dto.UpdateDTO;
import com.be.rebook.members.dto.UserinfoDTO;

import jakarta.validation.constraints.Pattern;

import org.springframework.http.ResponseEntity;

import com.be.rebook.common.argumentresolver.auth.Auth;
import com.be.rebook.common.argumentresolver.auth.MemberLoginInfo;
import com.be.rebook.common.config.BaseResponse;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/members")
public class MembersController {

    private final MemberService memberService;

    public MembersController(MemberService memberService) {
        this.memberService = memberService;
    }

    @GetMapping("/me")
    public ResponseEntity<BaseResponse<UserinfoDTO>> getMethodName(@Auth MemberLoginInfo memberLoginInfo) {
        UserinfoDTO userInfo = memberService.getUserinfo(memberLoginInfo.getUsername());
        return ResponseEntity.ok().body(new BaseResponse<>(userInfo));
    }

    @PatchMapping
    public ResponseEntity<BaseResponse<Members>> updateUser(@Auth MemberLoginInfo memberLoginInfo,
            @RequestBody UpdateDTO updateDTO) {
        return ResponseEntity.ok()
                .body(new BaseResponse<>(memberService.updateUser(memberLoginInfo.getUsername(), updateDTO)));
    }

    @DeleteMapping
    public ResponseEntity<BaseResponse<Members>> deleteUser(@Auth MemberLoginInfo memberLoginInfo) {
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.deleteUser(memberLoginInfo.getUsername())));
    }

    @GetMapping("/universities")
    public ResponseEntity<BaseResponse<List<String>>> searchUniversities(
            @RequestParam("unvToSearch") String unvToSearch) {
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getUniversitiesList(unvToSearch)));
    }

    @GetMapping("/majors")
    public ResponseEntity<BaseResponse<List<String>>> searchMajors(
            @Pattern(regexp = ".*[^가-힣\\sA-Z()].*", message = "검색어 입력이 잘못 되었습니다.") @RequestParam("majorToSearch") String majorToSearch) {
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getMajorsList(majorToSearch)));
    }
}
